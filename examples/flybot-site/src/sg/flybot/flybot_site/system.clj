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
   [sg.flybot.flybot-site.log :as log]
   [sg.flybot.flybot-site.oauth :as oauth]
   [sg.flybot.pullable.remote :as remote]
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
        (log/error "Unhandled exception:" (ex-message e))
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
  "Handle image uploads at /api/upload."
  [handler]
  (fn [request]
    (if (and (= :post (:request-method request))
             (= "/api/upload" (:uri request)))
      (do
        (let [dir (io/file uploads-dir)]
          (when-not (.exists dir) (.mkdirs dir)))
        (if-let [file (get-in request [:params "image"])]
          (let [ext (-> (:filename file) (str/split #"\.") last)
                filename (str (UUID/randomUUID) "." ext)
                dest-file (io/file uploads-dir filename)]
            (io/copy (:tempfile file) dest-file)
            (log/info "Image uploaded:" filename)
            (-> (resp/response (str "{\"url\":\"/uploads/" filename "\"}"))
                (resp/content-type "application/json")))
          (-> (resp/response "{\"error\":\"No image provided\"}")
              (resp/status 400)
              (resp/content-type "application/json"))))
      (handler request))))

(defn- wrap-dev-user
  "Inject fake session for dev mode."
  [handler dev-user]
  (if dev-user
    (fn [request]
      (let [session (merge (:session request)
                           {:user-email (:email dev-user)
                            :user-name (:name dev-user)
                            :user-picture (:picture dev-user)})]
        (handler (assoc request :session session))))
    handler))

(defn- session-timeout-handler [_]
  {:status 302 :headers {"Location" "/?error=session-expired"} :body ""})

;;=============================================================================
;; Port Selection
;;=============================================================================

(defn- try-start-server [app port]
  (try
    [(http-kit/run-server app {:port port}) port]
    (catch java.net.BindException _ nil)))

(defn- start-server-with-retry [app start-port max-attempts]
  (loop [port start-port, attempts 0]
    (if (>= attempts max-attempts)
      (throw (ex-info "Could not find available port" {:start-port start-port}))
      (if-let [result (try-start-server app port)]
        result
        (do (log/debug "Port" port "in use, trying" (inc port))
            (recur (inc port) (inc attempts)))))))

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
   (let [{:keys [server db auth session dev]} (cfg/prepare-cfg config)
         ;; Extract validated config values (defaults already applied)
         {:keys [port base-url]} server
         {:keys [backend path id bucket region]} db
         {:keys [owner-emails allowed-email-pattern
                 google-client-id google-client-secret]} auth
         {:keys [secret timeout]} session
         {:keys [mode? seed? backup-dir]} dev
         ;; Build Datahike config
         db-cfg (cond-> {:store {:backend backend}
                         :schema-flexibility :write
                         :keep-history? true}
                  id (assoc-in [:store :id] id)
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
       ::dev-mode? mode?

       ;;-----------------------------------------------------------------------
       ;; Database Connection
       ;; Note: We wrap conn in closeable and return a thunk to access it.
       ;; This prevents fun-map from auto-dereferencing (Connection is IDeref).
       ;; See: fun-map gotcha #1 - automatic dereferencing of IDeref values.
       ;;-----------------------------------------------------------------------
       ::db
       (fnk [::db-cfg]
            (log/info "Creating Datahike connection...")
            (let [conn (db/create-conn! db-cfg)
                  empty-db? (db/database-empty? conn)]
              (log/log-db-connected db-cfg)
              ;; Only seed/import if database is empty (important for persistent backends)
              (when empty-db?
                (cond
                  backup-dir
                  (let [result (backup/import-all! conn backup-dir)]
                    (log/info "Imported" (:count result) "posts from" backup-dir))
                  seed?
                  (do (db/seed! conn)
                      (log/log-db-seeded 10))))
              ;; Return closeable with thunk accessor (hibou pattern)
              (closeable {:conn (fn [] conn)
                          :cfg db-cfg}
                         #(do (log/info "Releasing Datahike connection...")
                              (db/release-conn! conn db-cfg)))))

       ;;-----------------------------------------------------------------------
       ;; API Function (request -> {:data :schema})
       ;;-----------------------------------------------------------------------
       ::api-fn
       (fnk [::db ::owner-emails]
            (log/debug "Initializing API function")
            (let [conn ((:conn db))]  ; Call thunk to get connection
              (if (seq owner-emails)
                (do (log/info "Role-based auth enabled for:" owner-emails)
                    (auth/make-api {:owner-emails owner-emails :conn conn}))
                (fn [_] {:data (api/make-api conn) :schema api/schema}))))

       ;;-----------------------------------------------------------------------
       ;; Session Configuration
       ;;-----------------------------------------------------------------------
       ::session-config
       (fnk [::dev-mode?]
            (let [key (or session-key
                          (do (log/warn "SESSION_SECRET not set - using random key")
                              (generate-session-key)))]
              {:store (cookie-store {:key key})
               :cookie-attrs {:same-site :lax
                              :http-only true
                              :secure (not dev-mode?)}}))

       ;;-----------------------------------------------------------------------
       ;; Dev User (auto-login for dev mode)
       ;;-----------------------------------------------------------------------
       ::dev-user
       (fnk [::dev-mode? ::owner-emails]
            (when (and dev-mode?
                       (not google-client-id)
                       (seq owner-emails))
              (let [email (first owner-emails)]
                (log/warn "DEV MODE: Auto-login as" email)
                {:email email
                 :name (first (str/split email #"@"))
                 :picture nil})))

       ;;-----------------------------------------------------------------------
       ;; Ring Application
       ;;-----------------------------------------------------------------------
       ::ring-app
       (fnk [::api-fn ::session-config ::dev-user ::base-url]
            (log/debug "Building Ring application")
            (-> (fn [_] (-> (resp/resource-response "index.html" {:root "public"})
                            (resp/content-type "text/html")))
                (wrap-resource "public")
                wrap-content-type
                (remote/wrap-api api-fn {:path "/api"})
                wrap-upload-route
                wrap-multipart-params
                wrap-keyword-params
                wrap-params
                oauth/wrap-logout
                (oauth/wrap-fetch-profile {:allowed-email-pattern email-pattern})
                (oauth/wrap-google-auth {:client-id google-client-id
                                         :client-secret google-client-secret
                                         :base-url base-url})
                (wrap-dev-user dev-user)
                (wrap-idle-session-timeout {:timeout timeout
                                            :timeout-handler session-timeout-handler})
                (wrap-session session-config)
                wrap-cors
                wrap-error-handler))

       ;;-----------------------------------------------------------------------
       ;; HTTP Server
       ;;-----------------------------------------------------------------------
       ::http-server
       (fnk [::ring-app ::port ::owner-emails]
            (let [[stop-fn actual-port] (start-server-with-retry ring-app port 10)]
              (log/log-startup actual-port)
              (println (str "Blog server started on port " actual-port))
              (println (str "  POST http://localhost:" actual-port "/api - Pull endpoint"))
              (println (str "  GET  http://localhost:" actual-port "/api/_schema - Schema"))
              (when google-client-id
                (println (str "  GET  http://localhost:" actual-port "/oauth2/google - Google sign-in")))
              (when (seq owner-emails)
                (println (str "  Owners: " (str/join ", " owner-emails))))
              (closeable {:port actual-port}
                         #(do (log/log-shutdown)
                              (stop-fn)
                              (println "Server stopped")))))}))))

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
  (def sys (make-system {:server {:port 18766}
                         :dev {:seed? true :mode? true}
                         :auth {:owner-emails "test@example.com"}}))

  ;; Access components (lazy - starts on demand)
  (::port sys) ;=> 18766
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
