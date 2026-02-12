(ns sg.flybot.flybot-site.server.system
  "System lifecycle using fun-map life-cycle-map.

   ## Key Concepts

   This module uses fun-map's associative dependency injection pattern:
   - `make-base-system` creates a production-ready system
   - `make-dev-system` and `make-dev-oauth-system` extend base via `assoc`
   - No mode conditionals inside components - just override components

   ## Usage

     ;; Create system (components not started yet)
     (def sys (make-system {:server {:port 8080}}))

     ;; Touch ::http-server to start the full chain
     (::http-server sys)

     ;; Gracefully stop everything
     (halt! sys)

   ## Modes

   - :prod (default) - Production system with secure cookies
   - :dev - Auto-login with configured user, insecure cookies
   - :dev-with-oauth2 - OAuth flow but insecure cookies

   ## Configuration

   Uses Malli-validated config (see cfg.cljc for schema):

     {:mode :dev  ; or :prod, :dev-with-oauth2
      :server {:port 8080 :base-url \"http://localhost:8080\"}
      :db {:backend :mem}  ; or :file, :s3
      :auth {:owner-emails \"admin@example.com\"}
      :session {:secret \"32-hex-chars\" :timeout 43200}
      :init {:seed? true}
      :dev {:user {:id \"...\" :name \"...\" :email \"...\"}}}"
  (:gen-class)
  (:require
   [sg.flybot.flybot-site.server.system.api :as api]

   [sg.flybot.flybot-site.server.system.cfg :as cfg]
   [sg.flybot.flybot-site.server.system.db :as db]
   [datahike-s3.core] ; registers :s3 backend
   [sg.flybot.flybot-site.server.system.auth :as auth]
   [sg.flybot.flybot-site.server.system.s3 :as s3]
   [sg.flybot.pullable.remote :as remote]
   [com.brunobonacci.mulog :as mu]
   [org.httpkit.server :as http-kit]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [ring.middleware.multipart-params :refer [wrap-multipart-params]]
   [ring.middleware.resource :refer [wrap-resource]]
   [ring.middleware.content-type :refer [wrap-content-type]]
   [ring.middleware.not-modified :refer [wrap-not-modified]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.middleware.session.cookie :refer [cookie-store]]
   [ring.middleware.session-timeout :refer [wrap-idle-session-timeout]]
   [ring.util.response :as resp]
   [robertluo.fun-map :refer [fnk life-cycle-map closeable halt!]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- generate-session-key
  "Generate random 16-byte key for dev mode."
  []
  (let [bytes (byte-array 16)]
    (.nextBytes (java.security.SecureRandom.) bytes)
    bytes))

;;=============================================================================
;; Asset Versioning
;;=============================================================================

(defn- read-js-path
  "Read shadow-cljs manifest.edn, return main module output filename."
  []
  (some-> (io/resource "public/js/manifest.edn")
          slurp edn/read-string
          (->> (filter #(= :main (:module-id %))) first :output-name)))

(defn- md5-hex [^String s]
  (let [bytes (.digest (java.security.MessageDigest/getInstance "MD5")
                       (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" %) bytes))))

(defn- asset-version [resource-path]
  (some-> (io/resource resource-path) slurp md5-hex (subs 0 8)))

(defn- read-version
  "Read app version from version.edn on classpath. Falls back to \"dev\"."
  []
  (or (some-> (io/resource "version.edn") slurp edn/read-string :version)
      "dev"))

(defn- render-index [js-path css-version app-version]
  (-> (io/resource "index-template.html") slurp
      (str/replace "{{main-js}}" (str "/js/" js-path))
      (str/replace "{{style-css}}" (str "/css/style.css?v=" css-version))
      (str/replace "{{version}}" app-version)))

;;=============================================================================
;; Middleware Components
;;=============================================================================

(defn- wrap-cors
  "Add CORS headers for cross-origin requests."
  [handler]
  (fn [request]
    (let [origin (get-in request [:headers "origin"] "*")]
      (if (= :options (:request-method request))
        {:status 204
         :headers {"Access-Control-Allow-Origin" origin
                   "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
                   "Access-Control-Allow-Headers" "Content-Type, Accept, Authorization"
                   "Access-Control-Allow-Credentials" "true"
                   "Access-Control-Max-Age" "86400"}}
        (-> (handler request)
            (assoc-in [:headers "Access-Control-Allow-Origin"] origin)
            (assoc-in [:headers "Access-Control-Allow-Credentials"] "true"))))))

(defn- wrap-cache-control
  "Set Cache-Control headers based on asset type.
   - HTML: no-store (always fresh)
   - Hashed JS (main.ABCD1234.js): immutable, max-age=1y
   - CSS: no-cache with ETag for 304 revalidation
   - Other: no-cache"
  [handler css-etag]
  (fn [request]
    (when-let [response (handler request)]
      (let [ct  (get-in response [:headers "Content-Type"] "")
            uri (or (:uri request) "")]
        (cond
          (str/includes? ct "text/html")
          (assoc-in response [:headers "Cache-Control"] "no-store")

          (re-find #"\.[a-fA-F0-9]{8}\.js$" uri)
          (assoc-in response [:headers "Cache-Control"]
                    "public, max-age=31536000, immutable")

          (and css-etag (str/ends-with? uri ".css"))
          (-> response
              (assoc-in [:headers "Cache-Control"] "no-cache")
              (assoc-in [:headers "ETag"] (str "\"" css-etag "\"")))

          :else
          (assoc-in response [:headers "Cache-Control"] "no-cache"))))))

(defn- wrap-error-handler
  "Safety net for unhandled exceptions. Returns generic error response.
   Normal errors flow as data through remote layer."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (mu/log ::unhandled-exception :error (ex-message e))
        (let [api-route? (str/starts-with? (or (:uri request) "") "/api")]
          (if api-route?
            (-> (resp/response "{\"error\":\"Internal server error\"}")
                (resp/status 500)
                (resp/content-type "application/json"))
            (-> (resp/response "<html><body><h1>500 - Server Error</h1></body></html>")
                (resp/status 500)
                (resp/content-type "text/html"))))))))

(defn- wrap-upload-route
  "Handle image uploads at /api/upload.

   Uses S3 if configured, otherwise falls back to local filesystem."
  [handler upload-handler]
  (fn [request]
    (if (and (= :post (:request-method request))
             (= "/api/upload" (:uri request)))
      (if-let [file (get-in request [:params "image"])]
        (try
          (let [result (upload-handler (:tempfile file) (:filename file))]
            (-> (resp/response (str "{\"url\":\"" (:url result) "\"}"))
                (resp/content-type "application/json")))
          (catch Exception e
            (mu/log ::upload-error :error (ex-message e))
            (-> (resp/response "{\"error\":\"Upload failed\"}")
                (resp/status 500)
                (resp/content-type "application/json"))))
        (-> (resp/response "{\"error\":\"No image provided\"}")
            (resp/status 400)
            (resp/content-type "application/json")))
      (handler request))))

(defn- wrap-dev-user
  "Inject fake session for dev mode.
   Includes roles from the dev-user map (defaults to all roles for dev convenience)."
  [handler dev-user]
  (if dev-user
    (fn [request]
      (let [session (merge (:session request)
                           {:user-id (:id dev-user)
                            :user-email (:email dev-user)
                            :user-name (:name dev-user)
                            :user-picture (:picture dev-user)
                            :roles (or (:roles dev-user) #{:member :admin :owner})})]
        (handler (assoc request :session session))))
    handler))

(defn- session-timeout-handler [_]
  {:status 302 :headers {"Location" "/?error=session-expired"} :body ""})

;;=============================================================================
;; System Definition
;;=============================================================================

(defn make-base-system
  "Create the base production blog system as a fun-map life-cycle-map.

   This is the production-ready system with no dev-mode conditionals.
   Use `make-dev-system` or `make-dev-oauth-system` for development.

   Components are lazily initialized. Access ::http-server to start.
   Call (halt! sys) to stop all components.

   Config is validated against Malli schema (see cfg.cljc).
   Defaults are applied automatically."
  [config]
  (let [{:keys [server db auth session init log uploads]} (cfg/prepare-cfg config)
        ;; Extract validated config values (defaults already applied)
        {:keys [port base-url]} server
        {:keys [backend path id bucket region]} db
        {uploads-type :type uploads-bucket :bucket uploads-region :region uploads-dir :dir} uploads
        {:keys [owner-emails allowed-email-pattern
                google-client-id google-client-secret]} auth
        {:keys [secret timeout]} session
        {:keys [seed?]} init
        {:keys [publishers context]} log
        ;; Build Datahike config
        ;; Note: S3 backend uses :store-id, mem/file use :id
        id-key (if (= backend :s3) :store-id :id)
        db-cfg (cond-> {:store {:backend backend}
                        :schema-flexibility :write
                        :keep-history? true}
                 id (assoc-in [:store id-key] id)
                 path (assoc-in [:store :path] path)
                 bucket (assoc-in [:store :bucket] bucket)
                 region (assoc-in [:store :region] region))
        ;; Parse special fields
        email-pattern (cfg/parse-email-pattern allowed-email-pattern)
        session-key (cfg/parse-session-secret secret)]

    (life-cycle-map
     {;;-----------------------------------------------------------------------
      ;; Config (plain values, no fnk needed)
      ;;-----------------------------------------------------------------------
      ::port port
      ::base-url base-url
      ::db-cfg db-cfg
      ::owner-emails owner-emails

      ;;-----------------------------------------------------------------------
      ;; Logger (mulog)
      ;;-----------------------------------------------------------------------
      ::logger
      (fnk []
           (when-let [filename (-> publishers first :filename)]
             (io/make-parents filename))
           (let [stop-fn (mu/start-publisher! {:type :multi :publishers publishers})]
             (when context (mu/set-global-context! context))
             (mu/log ::logger-started :publishers (mapv :type publishers))
             (closeable {:log (fn [event data] (mu/log event data))}
                        #(do (mu/log ::logger-stopping)
                             (Thread/sleep 100)
                             (stop-fn)))))

      ;;-----------------------------------------------------------------------
      ;; Database Connection
      ;; closeable wraps in a map, so fun-map won't auto-deref the conn inside.
      ;;-----------------------------------------------------------------------
      ::db
      (fnk [::db-cfg ::logger]
           (mu/log ::db-creating :config db-cfg)
           (let [conn (db/create-conn! db-cfg)
                 empty-db? (db/database-empty? conn)]
             (mu/log ::db-connected :backend (:backend db-cfg))
             (when (and empty-db? seed?)
               (db/seed! conn)
               (mu/log ::db-seeded :posts 10))
             (closeable {:conn conn :cfg db-cfg}
                        #(do (mu/log ::db-releasing)
                             (db/release-conn! conn db-cfg)))))

      ;;-----------------------------------------------------------------------
      ;; API Function (request -> {:data :schema})
      ;; Roles are now stored in session (initialized at OAuth login).
      ;; The api-fn reads roles from session, no owner-emails needed here.
      ;;-----------------------------------------------------------------------
      ::api-fn
      (fnk [::db ::logger]
           (mu/log ::api-fn-init)
           (let [conn (:conn db)]
             (api/make-api {:conn conn})))

      ;;-----------------------------------------------------------------------
      ;; Session Configuration (production: secure cookies)
      ;;-----------------------------------------------------------------------
      ::session-config
      (fnk [::logger]
           (let [key (or session-key
                         (do (mu/log ::session-secret-missing :using "random-key")
                             (generate-session-key)))]
             (mu/log ::session-store :type :cookie :secure true)
             {:store (cookie-store {:key key})
              :cookie-attrs {:same-site :lax
                             :http-only true
                             :secure true}}))

      ;;-----------------------------------------------------------------------
      ;; Upload Handler (S3 or local filesystem)
      ;;-----------------------------------------------------------------------
      ::upload-handler
      (fnk [::logger]
           (s3/make-upload-handler {:type uploads-type
                                    :bucket uploads-bucket
                                    :region uploads-region
                                    :dir uploads-dir}))

      ;;-----------------------------------------------------------------------
      ;; Dev User (nil in prod, overridden in dev mode)
      ;;-----------------------------------------------------------------------
      ::dev-user nil

      ;;-----------------------------------------------------------------------
      ;; Ring Application
      ;; Note: wrap-dev-user is a no-op when dev-user is nil
      ;;-----------------------------------------------------------------------
      ::ring-app
      (fnk [::api-fn ::session-config ::dev-user ::base-url ::upload-handler ::logger ::db ::owner-emails]
           (mu/log ::ring-app-building)
           (let [js-path    (or (read-js-path) "main.js")
                 css-ver    (or (asset-version "public/css/style.css") "0")
                 app-ver    (read-version)
                 index-html (render-index js-path css-ver app-ver)]
             (mu/log ::assets-resolved :js-path js-path :css-version css-ver :app-version app-ver)
             (-> (fn [_] (-> (resp/response index-html)
                             (resp/content-type "text/html")))
                 (wrap-resource "public")
                 wrap-content-type
                 (wrap-cache-control css-ver)
                 wrap-not-modified
                 (remote/wrap-api api-fn {:path "/api"})
                 (wrap-upload-route upload-handler)
                 wrap-multipart-params
                 auth/wrap-logout
                 (auth/wrap-oauth-success {:allowed-email-pattern email-pattern
                                           :owner-emails owner-emails
                                           :conn (:conn db)})
                 (auth/wrap-google-auth {:client-id google-client-id
                                         :client-secret google-client-secret
                                         :base-url base-url})
                 (wrap-dev-user dev-user)
                 (wrap-idle-session-timeout {:timeout timeout
                                             :timeout-handler session-timeout-handler})
                 (wrap-session session-config)
                 wrap-keyword-params
                 wrap-params
                 wrap-cors
                 wrap-error-handler)))

      ;;-----------------------------------------------------------------------
      ;; HTTP Server
      ;;-----------------------------------------------------------------------
      ::http-server
      (fnk [::ring-app ::port ::owner-emails ::logger]
           (let [stop-fn (http-kit/run-server ring-app {:port port})
                 info {:port port
                       :api-endpoint (str "http://localhost:" port "/api")
                       :schema-endpoint (str "http://localhost:" port "/api/_schema")
                       :oauth-endpoint (when google-client-id (str "http://localhost:" port "/oauth2/google"))
                       :owners owner-emails}]
             (mu/log ::server-started info)
             (closeable info
                        #(do (mu/log ::server-stopping)
                             (stop-fn)))))})))

(defn- make-dev-session-config
  "Create session config component with insecure cookies for development."
  [session-key]
  (fnk [::logger]
       (let [key (or session-key
                     (do (mu/log ::session-secret-missing :using "random-key")
                         (generate-session-key)))]
         (mu/log ::session-store :type :cookie :secure false)
         {:store (cookie-store {:key key})
          :cookie-attrs {:same-site :lax
                         :http-only true
                         :secure false}})))

(defn- make-dev-user-component
  "Create dev-user component that auto-logs in a configured user.
   Uses :roles from config, defaults to all roles for convenience."
  [dev-user-cfg]
  (fnk [::db ::logger]
       (when dev-user-cfg
         (let [conn (:conn db)
               {:keys [id name email roles]} dev-user-cfg
               roles (or roles #{:member :admin :owner})]
           (mu/log ::dev-mode-auto-login :name name :roles roles)
           (db/upsert-user! conn #:user{:id id
                                        :email email
                                        :name name
                                        :picture ""})
           ;; Grant configured roles
           (doseq [role roles]
             (db/grant-role! conn id role))
           {:id id
            :email email
            :name name
            :picture nil
            :roles roles}))))

(defn make-dev-system
  "Create dev system with auto-login user and insecure cookies.

   Extends base system via assoc - the fun-map associative DI pattern.
   Use for local development without OAuth flow."
  [config]
  (let [{:keys [session dev]} (cfg/prepare-cfg config)
        {:keys [secret]} session
        {dev-user-cfg :user} dev
        session-key (cfg/parse-session-secret secret)]
    (-> (make-base-system config)
        (assoc ::session-config (make-dev-session-config session-key))
        (assoc ::dev-user (make-dev-user-component dev-user-cfg)))))

(defn make-dev-oauth-system
  "Create dev system with OAuth but insecure cookies.

   Extends base system via assoc - the fun-map associative DI pattern.
   Use for testing OAuth flow locally (no auto-login)."
  [config]
  (let [{:keys [session]} (cfg/prepare-cfg config)
        {:keys [secret]} session
        session-key (cfg/parse-session-secret secret)]
    (-> (make-base-system config)
        (assoc ::session-config (make-dev-session-config session-key)))))

(defn make-system
  "Create the appropriate system based on :mode in config.

   Modes:
   - :prod (default) - Production system with secure cookies
   - :dev - Dev system with auto-login user and insecure cookies
   - :dev-with-oauth2 - Dev system with OAuth but insecure cookies"
  ([] (make-system {}))
  ([config]
   (let [{:keys [mode]} (cfg/prepare-cfg config)]
     (case mode
       :dev (make-dev-system config)
       :dev-with-oauth2 (make-dev-oauth-system config)
       (make-base-system config)))))

;;=============================================================================
;; REPL Convenience (stateful)
;;=============================================================================

(defonce ^:private system (atom nil))

(defn start!
  "Start server. Config merges with environment variables."
  ([] (start! {}))
  ([config]
   (when @system (halt! @system))
   (let [sys (make-system (cfg/apply-defaults (merge (cfg/config-from-env) config)))]
     (::http-server sys)  ; touch to start
     (reset! system sys))))

(defn stop!
  "Stop the server."
  []
  (when @system
    (halt! @system)
    (reset! system nil)))

(defn -main
  "Entry point. Starts system from environment variables."
  [& _args]
  (start!)
  @(promise))  ; Keep alive

(comment
  (require '[ring.mock.request :as mock])

  ;; Create dev system with test config
  (def sys (make-system (cfg/apply-defaults {:mode :dev
                                             :server {:port 18766}
                                             :init {:seed? true}
                                             :auth {:owner-emails "test@example.com"}})))

  ;; Access components (lazy - starts on demand)
  (::port sys) ;=> 18766

  ;; Start server
  (::http-server sys) ;=>> map?

  ;; Test API
  (let [app (::ring-app sys)
        req (mock/request :get "/api/_schema")]
    (:status (app req))) ;=> 200

  ;; Stop
  (halt! sys) ;=> nil

  ;; Test base system (production) - has secure cookies
  ;; Note: prod mode requires google-client-id, google-client-secret, session secret
  (def prod-sys (make-base-system (cfg/apply-defaults
                                   {:mode :prod
                                    :server {:port 18767}
                                    :init {:seed? true}
                                    :auth {:owner-emails "test@example.com"
                                           :google-client-id "test-id"
                                           :google-client-secret "test-secret"}
                                    :session {:secret "0123456789abcdef0123456789abcdef"}})))
  (::port prod-sys) ;=> 18767
  (get-in (::session-config prod-sys) [:cookie-attrs :secure]) ;=> true
  (halt! prod-sys) ;=> nil

  ;; Test dev-with-oauth2 system - has insecure cookies but no dev-user
  ;; Note: dev-with-oauth2 also requires OAuth credentials (it's testing the OAuth flow)
  (def oauth-sys (make-system (cfg/apply-defaults {:mode :dev-with-oauth2
                                                   :server {:port 18768}
                                                   :init {:seed? true}
                                                   :auth {:owner-emails "test@example.com"
                                                          :google-client-id "test-id"
                                                          :google-client-secret "test-secret"}})))
  (::port oauth-sys) ;=> 18768
  (get-in (::session-config oauth-sys) [:cookie-attrs :secure]) ;=> false
  (halt! oauth-sys) ;=> nil
  )
