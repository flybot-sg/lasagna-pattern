(ns sg.flybot.blog.system
  "System integration using fun-map life-cycle-map.

   The system is a single map managing all components:
   - :db - Database atom
   - :app - Ring handler
   - :server - HTTP server

   Usage:
     (def sys (make-system {:port 8080}))
     (:server sys)  ; starts server
     (halt! sys)    ; stops everything"
  (:require
   [sg.flybot.blog.api :as api]
   [sg.flybot.blog.db :as db]
   [sg.flybot.pullable.remote :as remote]
   [org.httpkit.server :as http-kit]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [robertluo.fun-map :refer [fnk life-cycle-map closeable halt!]]))

;;=============================================================================
;; API Component
;;=============================================================================

(defn make-api-fn
  "Create the API function that builds lazy API from request.

   The db is injected rather than using global state."
  [db-atom]
  (fn [ring-request]
    {:data (api/make-api db-atom {:params (:params ring-request)})
     :schema api/schema}))

;;=============================================================================
;; Ring App Component
;;=============================================================================

(defn make-app
  "Create Ring handler with pull API."
  [api-fn]
  (-> (fn [_] {:status 404 :body "Not Found"})
      (remote/wrap-api api-fn {:path "/api"})
      wrap-keyword-params
      wrap-params))

;;=============================================================================
;; System
;;=============================================================================

(defn make-system
  "Create the blog system.

   Options:
   - :port - HTTP port (default 8080)
   - :seed? - Seed database (default true)

   Returns a life-cycle-map. Access :server to start.
   Call (halt! sys) to stop."
  ([] (make-system {}))
  ([{:keys [port seed?] :or {port 8080 seed? true}}]
   (let [db-atom (atom {})]
     (when seed? (db/seed! db-atom))
     (life-cycle-map
      {:port port

       ;; Close over db-atom instead of putting in map
       ;; (life-cycle-map auto-derefs atoms)

       :api-fn
       (fnk []
            (make-api-fn db-atom))

       :app
       (fnk [api-fn]
            (make-app api-fn))

       :server
       (fnk [app port]
            (let [stop-fn (http-kit/run-server app {:port port})]
              (println "Blog server started on port" port)
              (println "  POST http://localhost:" port "/api")
              (println "  GET  http://localhost:" port "/api/_schema")
              (closeable {:port port}
                         (fn []
                           (stop-fn)
                           (reset! db-atom {})
                           (println "Server stopped")))))}))))

^:rct/test
(comment
  (require '[sg.flybot.pullable.remote.client :as client])

  ;; Create and start system
  (def sys (make-system {:port 8081}))
  (:server sys) ;=>> map?

  ;; Connect and query
  (def api (client/connect "http://localhost:8081/api"))
  (get-in (api '{:posts ?posts}) [:vars 'posts]) ;=>> seq?

  ;; Halt
  (halt! sys)) ;=> nil)
