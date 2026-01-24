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
;; API Handler
;;=============================================================================

(defn api-fn
  "Build API from Ring request.

   Returns the noun-only API with Posts collection.
   Uses the global db from db namespace.

   The Posts collection supports:
   - Sequential access via pattern: {:posts ?all}
   - Indexed lookup via ILookup: {:posts {{:id 3} ?post}}"
  [_ring-request]
  {:data (api/make-api db/db)
   :schema api/schema})

;;=============================================================================
;; Ring App
;;=============================================================================

(defn not-found-handler [_request]
  {:status 404
   :headers {"Content-Type" "text/plain"}
   :body "Not Found"})

(def app
  "Ring handler.

   Routes:
   - POST /api - Pull API endpoint
   - GET /api/_schema - Schema introspection"
  (-> not-found-handler
      (remote/wrap-api api-fn {:path "/api"})
      wrap-keyword-params
      wrap-params))

;;=============================================================================
;; Server Lifecycle
;;=============================================================================

(defonce ^:private server (atom nil))

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
   (when seed?
     (db/seed!))
   (reset! server (http-kit/run-server app {:port port}))
   (println "Blog server started on port" port)
   (println "  POST http://localhost:" port "/api - Pull endpoint")
   (println "  GET  http://localhost:" port "/api/_schema - Schema")
   {:port port}))

(defn stop!
  "Stop the blog server."
  []
  (when-let [s @server]
    (s)  ; http-kit stop fn
    (reset! server nil)
    (println "Server stopped")))

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
  (db/seed!)

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
  (db/reset-db!))
