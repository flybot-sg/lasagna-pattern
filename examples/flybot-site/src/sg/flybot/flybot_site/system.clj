(ns sg.flybot.flybot-site.system
  "System integration using fun-map life-cycle-map.

   ## Usage

     (def sys (make-system {:port 8080}))
     (:server sys)  ; starts server
     (halt! sys)    ; stops everything

   ## With Role-Based Auth

     (def sys (make-system {:port 8080
                            :owner-emails #{\"me@gmail.com\"}}))

   When :owner-emails is provided, schema varies by session:
   - Session with owner email → full CRUD + :me endpoint
   - No session or non-owner → read-only"
  (:require
   [sg.flybot.flybot-site.api :as api]
   [sg.flybot.flybot-site.auth :as auth]
   [sg.flybot.flybot-site.backup :as backup]
   [sg.flybot.flybot-site.db :as db]
   [sg.flybot.flybot-site.log :as log]
   [sg.flybot.pullable.remote :as remote]
   [org.httpkit.server :as http-kit]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [ring.middleware.session :refer [wrap-session]]
   [robertluo.fun-map :refer [fnk life-cycle-map closeable halt!]]))

;;=============================================================================
;; Port Selection
;;=============================================================================

(defn- try-start-server
  "Try to start http-kit on port. Returns [stop-fn port] on success, nil on failure."
  [app port]
  (try
    [(http-kit/run-server app {:port port}) port]
    (catch java.net.BindException _
      nil)))

(defn- start-server-with-retry
  "Start server, trying ports from start-port up to start-port + max-attempts.
   Returns [stop-fn actual-port] or throws if all ports fail."
  [app start-port max-attempts]
  (loop [port start-port
         attempts 0]
    (if (>= attempts max-attempts)
      (throw (ex-info "Could not find available port"
                      {:start-port start-port :attempts max-attempts}))
      (if-let [result (try-start-server app port)]
        result
        (do
          (log/debug "Port" port "in use, trying" (inc port))
          (recur (inc port) (inc attempts)))))))

;;=============================================================================
;; System
;;=============================================================================

(defn make-system
  "Create the blog system.

   Options:
   - :port - HTTP port (default 8080)
   - :seed? - Seed database with sample data (default true, ignored if :backup-dir)
   - :backup-dir - Directory to import posts from (optional, overrides :seed?)
   - :owner-emails - Set of owner emails for role-based auth (optional)
   - :db-cfg - Datahike config (default: in-memory)

   Returns a life-cycle-map. Access :server to start.
   Call (halt! sys) to stop."
  ([] (make-system {}))
  ([{:keys [port seed? backup-dir owner-emails db-cfg] :or {port 8080 seed? true}}]
   (log/info "Creating blog system...")
   (let [cfg (or db-cfg db/default-cfg)
         conn (db/create-conn! cfg)]
     (log/log-db-connected cfg)
     (cond
       backup-dir
       (let [result (backup/import-all! conn backup-dir)]
         (log/info "Imported" (:count result) "posts from" backup-dir))

       seed?
       (do (db/seed! conn)
           (log/log-db-seeded 10)))
     (when owner-emails
       (log/info "Role-based auth enabled for:" owner-emails))
     (life-cycle-map
      {:port port

       :api-fn
       (fnk []
            (log/debug "Initializing API function")
            (if owner-emails
              (auth/make-api {:owner-emails owner-emails :conn conn})
              (fn [_] {:data (api/make-api conn) :schema api/schema})))

       :app
       (fnk [api-fn]
            (log/debug "Building Ring app")
            (cond-> (fn [_] {:status 404 :body "Not Found"})
              true (remote/wrap-api api-fn {:path "/api"})
              owner-emails wrap-session
              true wrap-keyword-params
              true wrap-params))

       :server
       (fnk [app port]
            (let [[stop-fn actual-port] (start-server-with-retry app port 10)]
              (log/log-startup actual-port)
              (when (not= port actual-port)
                (log/info "Requested port" port "was in use, using" actual-port))
              (println "Blog server started on port" actual-port)
              (println "  POST http://localhost:" actual-port "/api")
              (println "  GET  http://localhost:" actual-port "/api/_schema")
              (closeable {:port actual-port}
                         (fn []
                           (log/log-shutdown)
                           (stop-fn)
                           (db/release-conn! conn cfg)
                           (log/info "Server stopped")
                           (println "Server stopped")))))}))))

^:rct/test
(comment
  (def sys (make-system {:port 18765 :seed? true}))
  (:server sys) ;=>> map?

  (let [api-fn (:api-fn sys)
        {:keys [data schema]} (api-fn {})]
    [(count (seq (:posts data))) (contains? schema :posts)])
  ;=> [10 true]

  (halt! sys) ;=> nil
  ) 
