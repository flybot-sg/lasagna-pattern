(ns sg.flybot.blog.server
  "HTTP server for blog API.

   Start the server:
     (start!)

   Stop the server:
     (stop!)"
  (:require
   [sg.flybot.blog.api :as api]
   [sg.flybot.blog.db :as db]
   [sg.flybot.pullable.remote :as remote]
   [org.httpkit.server :as http-kit]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]))

;;=============================================================================
;; State
;;=============================================================================

(defonce ^:private server (atom nil))
(defonce ^:private conn (atom nil))

;;=============================================================================
;; API Handler
;;=============================================================================

(defn make-api-fn
  "Build API function from Datahike connection.

   Returns a function that takes a Ring request and returns
   the noun-only API with Posts collection.

   The Posts collection supports:
   - Sequential access via pattern: {:posts ?all}
   - Indexed lookup via ILookup: {:posts {{:id 3} ?post}}"
  [conn]
  (fn [_ring-request]
    {:data (api/make-api conn)
     :schema api/schema}))

;;=============================================================================
;; Ring App
;;=============================================================================

(defn not-found-handler [_request]
  {:status 404
   :headers {"Content-Type" "text/plain"}
   :body "Not Found"})

(defn make-app
  "Create Ring handler with given API function.

   Routes:
   - POST /api - Pull API endpoint
   - GET /api/_schema - Schema introspection"
  [api-fn]
  (-> not-found-handler
      (remote/wrap-api api-fn {:path "/api"})
      wrap-keyword-params
      wrap-params))

;;=============================================================================
;; Server Lifecycle
;;=============================================================================

(declare stop!)

(defn start!
  "Start the blog server.

   Options:
   - :port - HTTP port (default 8080)
   - :seed? - Seed database with sample data (default true)"
  ([]
   (start! {}))
  ([{:keys [port seed?] :or {port 8080 seed? true}}]
   (when @server
     (stop!))
   (reset! conn (db/create-conn!))
   (when seed?
     (db/seed! @conn))
   (let [app (make-app (make-api-fn @conn))]
     (reset! server (http-kit/run-server app {:port port})))
   (println "Blog server started on port" port)
   (println "  POST http://localhost:" port "/api - Pull endpoint")
   (println "  GET  http://localhost:" port "/api/_schema - Schema")
   {:port port}))

(defn stop!
  "Stop the blog server."
  []
  (when-let [s @server]
    (s)  ; http-kit stop fn
    (reset! server nil))
  (when-let [c @conn]
    (db/release-conn! c)
    (reset! conn nil))
  (println "Server stopped"))

(defn restart!
  "Restart the server."
  []
  (stop!)
  (start!))

^:rct/test
(comment
  (require '[ring.mock.request :as mock]
           '[sg.flybot.pullable.remote.http :as http])

  ;; Setup
  (def test-conn (db/create-conn!))
  (db/seed! test-conn)
  (def app (make-app (make-api-fn test-conn)))

  ;; Test API schema endpoint
  (:status (app (-> (mock/request :get "/api/_schema")
                    (mock/header "Accept" "application/transit+json")))) ;=> 200

  ;; Test pull request
  (let [req (-> (mock/request :post "/api")
                (mock/header "Content-Type" "application/transit+json")
                (mock/header "Accept" "application/transit+json")
                (mock/body (http/encode {:pattern '{:posts ?posts}} :transit-json)))
        resp (app req)]
    (:status resp)) ;=> 200

  ;; Cleanup
  (db/release-conn! test-conn))
