(ns sg.flybot.blog.server
  "HTTP server for blog API.

   Start the server:
     (start!)

   Stop the server:
     (stop!)"
  (:require
   [sg.flybot.blog.api :as api]
   [sg.flybot.blog.db :as db]
   [sg.flybot.blog.log :as log]
   [sg.flybot.pullable.remote :as remote]
   [org.httpkit.server :as http-kit]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [ring.middleware.multipart-params :refer [wrap-multipart-params]]
   [ring.middleware.resource :refer [wrap-resource]]
   [ring.middleware.content-type :refer [wrap-content-type]]
   [ring.util.response :as resp]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.util UUID]))

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
;; Image Upload
;;=============================================================================

(def ^:private uploads-dir "resources/public/uploads")

(defn- ensure-uploads-dir! []
  (let [dir (io/file uploads-dir)]
    (when-not (.exists dir)
      (.mkdirs dir))))

(defn- upload-image-handler
  "Handle image upload. Returns JSON with the image URL."
  [request]
  (ensure-uploads-dir!)
  (if-let [file (get-in request [:params "image"])]
    (let [ext (-> (:filename file) (str/split #"\.") last)
          filename (str (UUID/randomUUID) "." ext)
          dest-file (io/file uploads-dir filename)]
      (log/debug "Uploading image:" (:filename file) "size:" (.length (:tempfile file)))
      (io/copy (:tempfile file) dest-file)
      (log/info "Image uploaded:" filename)
      (-> (resp/response (str "{\"url\":\"/uploads/" filename "\"}"))
          (resp/content-type "application/json")))
    (do
      (log/warn "Image upload failed: no image provided")
      (-> (resp/response "{\"error\":\"No image provided\"}")
          (resp/status 400)
          (resp/content-type "application/json")))))

;;=============================================================================
;; Ring App
;;=============================================================================

(defn- index-handler [_request]
  (-> (resp/resource-response "index.html" {:root "public"})
      (resp/content-type "text/html")))

(defn- wrap-upload-route
  "Add image upload route."
  [handler]
  (fn [request]
    (if (and (= :post (:request-method request))
             (= "/api/upload" (:uri request)))
      (upload-image-handler request)
      (handler request))))

(defn make-app
  "Create Ring handler with given API function.

   Routes:
   - POST /api - Pull API endpoint
   - POST /api/upload - Image upload endpoint
   - GET /api/_schema - Schema introspection
   - GET /* - Static files from resources/public"
  [api-fn]
  (-> index-handler
      (wrap-resource "public")
      wrap-content-type
      (remote/wrap-api api-fn {:path "/api"})
      wrap-upload-route
      wrap-multipart-params
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
   (log/info "Starting blog server...")
   (reset! conn (db/create-conn!))
   (log/log-db-connected db/default-cfg)
   (when seed?
     (db/seed! @conn)
     (log/log-db-seeded 3))
   (let [app (make-app (make-api-fn @conn))]
     (reset! server (http-kit/run-server app {:port port})))
   (log/log-startup port)
   (println "Blog server started on port" port)
   (println "  POST http://localhost:" port "/api - Pull endpoint")
   (println "  GET  http://localhost:" port "/api/_schema - Schema")
   {:port port}))

(defn stop!
  "Stop the blog server."
  []
  (log/log-shutdown)
  (when-let [s @server]
    (s)  ; http-kit stop fn
    (reset! server nil))
  (when-let [c @conn]
    (db/release-conn! c)
    (reset! conn nil))
  (log/info "Server stopped")
  (println "Server stopped"))

(defn restart!
  "Restart the server."
  []
  (stop!)
  (start!))

(defn -main
  "Entry point for running server from command line."
  [& _args]
  (start!)
  ;; Keep the main thread alive
  @(promise))

^:rct/test
(comment
  (require '[ring.mock.request :as mock]
           '[sg.flybot.pullable.remote.http :as http]
           '[clojure.java.io :as io])

  ;; Setup
  (def test-conn (db/create-conn!))
  (db/seed! test-conn)
  (def app (make-app (make-api-fn test-conn)))

  (defn pull [pattern]
    (let [req (-> (mock/request :post "/api")
                  (mock/header "Content-Type" "application/transit+json")
                  (mock/header "Accept" "application/transit+json")
                  (mock/body (http/encode {:pattern pattern} :transit-json)))
          resp (app req)]
      (http/decode (.readAllBytes (io/input-stream (:body resp))) :transit-json)))

  ;; Schema endpoint
  (:status (app (-> (mock/request :get "/api/_schema")
                    (mock/header "Accept" "application/transit+json")))) ;=> 200

  ;; LIST - response is {posts [...]}
  (count (get (pull '{:posts ?posts}) 'posts)) ;=> 3

  ;; CREATE - returns created post (content has frontmatter, author extracted)
  (let [result (get (pull '{:posts {nil {:post/title "New" :post/content "---\nauthor: A\n---\n\nC"}}}) 'posts)]
    [(:post/title result) (:post/author result)])
  ;=> ["New" "A"]

  ;; Cleanup
  (db/release-conn! test-conn))
