(ns sg.flybot.flybot-site.system
  "System lifecycle using fun-map life-cycle-map.

   ## Key Concepts

   This module demonstrates fun-map for dependency injection and lifecycle management.
   Components are lazily initialized and dependencies are automatically resolved.

   ## Usage

     ;; Create system (components not started yet)
     (def sys (make-system {:server {:port 8080}}))

     ;; Touch :http-server to start the full chain
     (:http-server sys)

     ;; Gracefully stop everything
     (halt! sys)

   ## Configuration

     {:server {:port 8080 :base-url \"http://localhost:8080\"}
      :db {:backend :mem}  ; or :file, :s3
      :auth {:owner-emails \"admin@example.com\"}
      :session {:secret \"32-hex-chars\" :timeout 43200}
      :dev {:mode? false :seed? true}}"
  (:require
   [sg.flybot.flybot-site.api :as api]
   [sg.flybot.flybot-site.auth :as auth]
   [sg.flybot.flybot-site.backup :as backup]
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

;;=============================================================================
;; Config Parsing
;;=============================================================================

(defn- parse-owner-emails
  "Parse comma-separated emails to set."
  [s]
  (if (or (nil? s) (str/blank? s))
    #{}
    (set (map str/trim (str/split s #",")))))

(defn- parse-email-pattern
  "Parse regex pattern string."
  [s]
  (when (and s (not (str/blank? s)))
    (re-pattern s)))

(defn- parse-session-secret
  "Parse 32 hex chars to 16-byte array."
  [s]
  (when (and s (not (str/blank? s)))
    (let [hex-chars (str/replace s #"[^0-9a-fA-F]" "")]
      (when (not= 32 (count hex-chars))
        (throw (ex-info "Session secret must be 32 hex characters (16 bytes)"
                        {:provided-length (count hex-chars)})))
      (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 16))
                       (partition 2 hex-chars))))))

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

   Components are lazily initialized. Access :http-server to start.
   Call (halt! sys) to stop all components.

   Config keys:
   - :server - {:port :base-url}
   - :db - {:backend :mem|:file|:s3, :path, :bucket, :region}
   - :auth - {:google-client-id :google-client-secret :owner-emails :allowed-email-pattern}
   - :session - {:secret :timeout}
   - :dev - {:mode? :seed? :backup-dir}"
  ([] (make-system {}))
  ([config]
   (let [{:keys [server db auth session dev]
          :or {server {} db {} auth {} session {} dev {}}} config
         ;; Parse config
         port (or (:port server) 8080)
         base-url (or (:base-url server) "http://localhost:8080")
         db-cfg (merge db/default-cfg
                       (when-let [backend (:backend db)] {:store {:backend backend}})
                       (when-let [path (:path db)] {:store {:path path}})
                       (when-let [id (:id db)] {:store {:id id}}))
         owner-emails (parse-owner-emails (:owner-emails auth))
         allowed-email-pattern (parse-email-pattern (:allowed-email-pattern auth))
         session-secret (parse-session-secret (:secret session))
         session-timeout (or (:timeout session) 43200)
         dev-mode? (:mode? dev)
         seed? (if (contains? dev :seed?) (:seed? dev) true)
         backup-dir (:backup-dir dev)]

     (life-cycle-map
      {;;-----------------------------------------------------------------------
       ;; Config (plain values, no fnk needed)
       ;;-----------------------------------------------------------------------
       ::port port
       ::base-url base-url
       ::db-cfg db-cfg
       ::owner-emails owner-emails
       ::dev-mode? dev-mode?

       ;;-----------------------------------------------------------------------
       ;; Database Connection
       ;; Note: We wrap conn in closeable and return a thunk to access it.
       ;; This prevents fun-map from auto-dereferencing (Connection is IDeref).
       ;; See: fun-map gotcha #1 - automatic dereferencing of IDeref values.
       ;;-----------------------------------------------------------------------
       ::db
       (fnk [::db-cfg]
            (log/info "Creating Datahike connection...")
            (let [conn (db/create-conn! db-cfg)]
              (log/log-db-connected db-cfg)
              (cond
                backup-dir
                (let [result (backup/import-all! conn backup-dir)]
                  (log/info "Imported" (:count result) "posts from" backup-dir))
                seed?
                (do (db/seed! conn)
                    (log/log-db-seeded 10)))
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
            (let [key (or session-secret
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
                       (not (:google-client-id auth))
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
                (oauth/wrap-fetch-profile {:allowed-email-pattern allowed-email-pattern})
                (oauth/wrap-google-auth {:client-id (:google-client-id auth)
                                         :client-secret (:google-client-secret auth)
                                         :base-url base-url})
                (wrap-dev-user dev-user)
                (wrap-idle-session-timeout {:timeout session-timeout
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
              (when (:google-client-id auth)
                (println (str "  GET  http://localhost:" actual-port "/oauth2/google - Google sign-in")))
              (when (seq owner-emails)
                (println (str "  Owners: " (str/join ", " owner-emails))))
              (closeable {:port actual-port}
                         #(do (log/log-shutdown)
                              (stop-fn)
                              (println "Server stopped")))))}))))

;;=============================================================================
;; Environment Config
;;=============================================================================

(defn- get-env
  ([k] (System/getenv k))
  ([k default] (or (System/getenv k) default)))

(defn config-from-env
  "Build config map from environment variables.

   Environment variables:
   - BLOG_PORT, BLOG_BASE_URL
   - DATAHIKE_BACKEND (:mem, :file, :s3), DATAHIKE_PATH, DATAHIKE_BUCKET, DATAHIKE_REGION
   - GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET
   - BLOG_OWNER_EMAILS, BLOG_ALLOWED_EMAILS
   - SESSION_SECRET, SESSION_TIMEOUT
   - BLOG_DEV_MODE, BLOG_BACKUP_DIR"
  []
  {:server {:port (some-> (get-env "BLOG_PORT") parse-long)
            :base-url (get-env "BLOG_BASE_URL")}
   :db {:backend (some-> (get-env "DATAHIKE_BACKEND") keyword)
        :path (get-env "DATAHIKE_PATH")
        :bucket (get-env "DATAHIKE_BUCKET")
        :region (get-env "DATAHIKE_REGION")}
   :auth {:google-client-id (get-env "GOOGLE_CLIENT_ID")
          :google-client-secret (get-env "GOOGLE_CLIENT_SECRET")
          :owner-emails (get-env "BLOG_OWNER_EMAILS")
          :allowed-email-pattern (get-env "BLOG_ALLOWED_EMAILS")}
   :session {:secret (get-env "SESSION_SECRET")
             :timeout (some-> (get-env "SESSION_TIMEOUT") parse-long)}
   :dev {:mode? (= "true" (get-env "BLOG_DEV_MODE"))
         :seed? (not= "false" (get-env "BLOG_SEED"))
         :backup-dir (get-env "BLOG_BACKUP_DIR")}})

;;=============================================================================
;; REPL Convenience (stateful)
;;=============================================================================

(defonce ^:private system (atom nil))

(defn start!
  "Start server. Config merges with environment variables."
  ([] (start! {}))
  ([config]
   (when @system (halt! @system))
   (let [sys (make-system (merge (config-from-env) config))]
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
