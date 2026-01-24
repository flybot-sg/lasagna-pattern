(ns sg.flybot.blog.system
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
   [sg.flybot.blog.api :as api]
   [sg.flybot.blog.auth :as auth]
   [sg.flybot.blog.db :as db]
   [sg.flybot.pullable.remote :as remote]
   [org.httpkit.server :as http-kit]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [ring.middleware.session :refer [wrap-session]]
   [robertluo.fun-map :refer [fnk life-cycle-map closeable halt!]]))

;;=============================================================================
;; System
;;=============================================================================

(defn make-system
  "Create the blog system.

   Options:
   - :port - HTTP port (default 8080)
   - :seed? - Seed database (default true)
   - :owner-emails - Set of owner emails for role-based auth (optional)
   - :db-cfg - Datahike config (default: in-memory)

   Returns a life-cycle-map. Access :server to start.
   Call (halt! sys) to stop."
  ([] (make-system {}))
  ([{:keys [port seed? owner-emails db-cfg] :or {port 8080 seed? true}}]
   (let [cfg (or db-cfg db/default-cfg)
         conn (db/create-conn! cfg)]
     (when seed? (db/seed! conn))
     (life-cycle-map
      {:port port

       :api-fn
       (fnk []
            (if owner-emails
              (auth/make-api {:owner-emails owner-emails :conn conn})
              (fn [_] {:data (api/make-api conn) :schema api/schema})))

       :app
       (fnk [api-fn]
            (cond-> (fn [_] {:status 404 :body "Not Found"})
              true (remote/wrap-api api-fn {:path "/api"})
              owner-emails wrap-session
              true wrap-keyword-params
              true wrap-params))

       :server
       (fnk [app port]
            (let [stop-fn (http-kit/run-server app {:port port})]
              (println "Blog server started on port" port)
              (println "  POST http://localhost:" port "/api")
              (println "  GET  http://localhost:" port "/api/_schema")
              (closeable {:port port}
                         (fn []
                           (stop-fn)
                           (db/release-conn! conn cfg)
                           (println "Server stopped")))))}))))

^:rct/test
(comment
  (def sys (make-system {:port 18765 :seed? true}))
  (:server sys) ;=>> map?

  (let [api-fn (:api-fn sys)
        {:keys [data schema]} (api-fn {})]
    [(count (seq (:posts data))) (contains? schema :posts)])
  ;=> [3 true]

  (halt! sys)) ;=> nil)
