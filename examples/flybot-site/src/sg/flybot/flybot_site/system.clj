(ns sg.flybot.flybot-site.system
  "System lifecycle using fun-map life-cycle-map.

   ## Key Concepts

   This module demonstrates fun-map for dependency injection and lifecycle management.
   Components are lazily initialized and dependencies are automatically resolved.

   ## Usage

     ;; Create system (components not started yet)
     (def sys (make-system {:server {:port 8080}}))

     ;; Touch ::http-server to start the full chain
     (::http-server sys)

     ;; Gracefully stop everything
     (halt! sys)

   ## Configuration

   Uses Malli-validated config (see cfg.cljc for schema):

     {:server {:port 8080 :base-url \"http://localhost:8080\"}
      :db {:backend :mem}  ; or :file, :s3
      :auth {:owner-emails \"admin@example.com\"}
      :session {:secret \"32-hex-chars\" :timeout 43200}
      :dev {:mode? false :seed? true}}"
  (:require
   [sg.flybot.flybot-site.api :as api]
   [sg.flybot.flybot-site.auth :as auth]
   [sg.flybot.flybot-site.backup :as backup]
   [sg.flybot.flybot-site.cfg :as cfg]
   [sg.flybot.flybot-site.db :as db]
   [sg.flybot.flybot-site.oauth :as oauth]
   [sg.flybot.flybot-site.s3 :as s3]
   [sg.flybot.pullable.remote :as remote]
   [com.brunobonacci.mulog :as mu]
   [org.httpkit.server :as http-kit]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [ring.middleware.multipart-params :refer [wrap-multipart-params]]
   [ring.middleware.resource :refer [wrap-resource]]
   [ring.middleware.content-type :refer [wrap-content-type]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.middleware.session.cookie :refer [cookie-store]]
   [ring.middleware.session-timeout :refer [wrap-idle-session-timeout]]
   [ring.util.response :as resp]
   [robertluo.fun-map :refer [fnk life-cycle-map closeable halt!]]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.util UUID]))

(defn- generate-session-key
  "Generate random 16-byte key for dev mode."
  []
  (let [bytes (byte-array 16)]
    (.nextBytes (java.security.SecureRandom.) bytes)
    bytes))

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

(defn- wrap-error-handler
  "Catch unhandled exceptions, return JSON/HTML error."
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

(def ^:private uploads-dir "resources/public/uploads")

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
  "Inject fake session for dev mode."
  [handler dev-user]
  (if dev-user
    (fn [request]
      (let [session (merge (:session request)
                           {:user-id (:id dev-user)
                            :user-email (:email dev-user)
                            :user-name (:name dev-user)
                            :user-picture (:picture dev-user)})]
        (handler (assoc request :session session))))
    handler))

(defn- session-timeout-handler [_]
  {:status 302 :headers {"Location" "/?error=session-expired"} :body ""})

;;=============================================================================
;; System Definition
;;=============================================================================

(defn make-system
  "Create the blog system as a fun-map life-cycle-map.

   Components are lazily initialized. Access ::http-server to start.
   Call (halt! sys) to stop all components.

   Config is validated against Malli schema (see cfg.cljc).
   Defaults are applied automatically."
  ([] (make-system {}))
  ([config]
   (let [{:keys [mode server db auth session init log uploads]} (cfg/prepare-cfg config)
         ;; Extract validated config values (defaults already applied)
         {:keys [port base-url]} server
         {:keys [backend path id bucket region]} db
         {uploads-type :type uploads-bucket :bucket uploads-region :region uploads-dir :dir} uploads
         {:keys [owner-emails allowed-email-pattern
                 google-client-id google-client-secret]} auth
         {:keys [secret timeout]} session
         {:keys [seed? backup-dir]} init
         {:keys [publishers context]} log
         dev-mode? (boolean (#{:dev :dev-with-oauth2} mode))
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
       ::mode mode
       ::dev-mode? dev-mode?

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
              (when empty-db?
                (cond
                  backup-dir
                  (let [result (backup/import-all! conn backup-dir)]
                    (mu/log ::db-imported :count (:count result) :from backup-dir))
                  seed?
                  (do (db/seed! conn)
                      (mu/log ::db-seeded :posts 10))))
              (closeable {:conn conn :cfg db-cfg}
                         #(do (mu/log ::db-releasing)
                              (db/release-conn! conn db-cfg)))))

       ;;-----------------------------------------------------------------------
       ;; API Function (request -> {:data :schema})
       ;;-----------------------------------------------------------------------
       ::api-fn
       (fnk [::db ::owner-emails ::logger]
            (mu/log ::api-fn-init)
            (let [conn (:conn db)]
              (if (seq owner-emails)
                (do (mu/log ::role-auth-enabled :owner-emails owner-emails)
                    (auth/make-api {:owner-emails owner-emails :conn conn}))
                (fn [_] {:data (api/make-api conn) :schema api/schema}))))

       ;;-----------------------------------------------------------------------
       ;; Session Configuration
       ;; Uses encrypted cookie-store for stateless sessions (survives container restarts)
       ;;-----------------------------------------------------------------------
       ::session-config
       (fnk [::dev-mode? ::logger]
            (let [key (or session-key
                          (do (mu/log ::session-secret-missing :using "random-key")
                              (generate-session-key)))]
              (mu/log ::session-store :type :cookie)
              {:store (cookie-store {:key key})
               :cookie-attrs {:same-site :lax
                              :http-only true
                              :secure (not dev-mode?)}}))

       ;;-----------------------------------------------------------------------
       ;; Dev User (auto-login for :dev mode only, not :dev-with-oauth2)
       ;;-----------------------------------------------------------------------
       ::dev-user
       (fnk [::mode ::owner-emails ::db ::logger]
            (when (and (= :dev mode)
                       (seq owner-emails))
              (let [conn (:conn db)
                    email (first owner-emails)
                    ;; Use sample-alice to match seed data (same slug for filtering)
                    user-id "sample-alice"
                    user-name "Alice Johnson"]
                (mu/log ::dev-mode-auto-login :email email :user-id user-id)
                ;; Upsert ensures user exists with correct data
                (db/upsert-user! conn #:user{:id user-id
                                             :email email
                                             :name user-name
                                             :picture ""})
                {:id user-id
                 :email email
                 :name user-name
                 :picture nil})))

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
       ;; Ring Application
       ;;-----------------------------------------------------------------------
       ::ring-app
       (fnk [::api-fn ::session-config ::dev-user ::base-url ::upload-handler ::logger ::db]
            (mu/log ::ring-app-building)
            (-> (fn [_] (-> (resp/resource-response "index.html" {:root "public"})
                            (resp/content-type "text/html")))
                (wrap-resource "public")
                wrap-content-type
                (remote/wrap-api api-fn {:path "/api"})
                (wrap-upload-route upload-handler)
                wrap-multipart-params
                oauth/wrap-logout
                (oauth/wrap-oauth-success {:allowed-email-pattern email-pattern
                                           :conn (:conn db)})
                (oauth/wrap-google-auth {:client-id google-client-id
                                         :client-secret google-client-secret
                                         :base-url base-url})
                (wrap-dev-user dev-user)
                (wrap-idle-session-timeout {:timeout timeout
                                            :timeout-handler session-timeout-handler})
                (wrap-session session-config)
                wrap-keyword-params
                wrap-params
                wrap-cors
                wrap-error-handler))

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
                              (stop-fn)))))}))))

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

(defn restart! [] (stop!) (start!))

(defn -main
  "Entry point. Starts system from environment variables."
  [& _args]
  (start!)
  @(promise))  ; Keep alive

^:rct/test
(comment
  (require '[ring.mock.request :as mock])

  ;; Create system with test config
  (def sys (make-system {:mode :dev
                         :server {:port 18766}
                         :init {:seed? true}
                         :auth {:owner-emails "test@example.com"}}))

  ;; Access components (lazy - starts on demand)
  (::port sys) ;=> 18766
  (::mode sys) ;=> :dev
  (::dev-mode? sys) ;=> true

  ;; Start server
  (::http-server sys) ;=>> map?

  ;; Test API
  (let [app (::ring-app sys)
        req (mock/request :get "/api/_schema")]
    (:status (app req))) ;=> 200

  ;; Stop
  (halt! sys) ;=> nil
  )
