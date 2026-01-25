(ns sg.flybot.flybot-site.server
  "HTTP server for blog API.

   Start the server:
     (start!)

   Stop the server:
     (stop!)"
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
   [ring.middleware.session.memory :refer [memory-store]]
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
   - Indexed lookup via ILookup: {:posts {{:id 3} ?post}}

   Options:
   - :owner-emails - Set of owner email addresses for full CRUD access"
  ([conn] (make-api-fn conn {}))
  ([conn {:keys [owner-emails] :or {owner-emails #{}}}]
   (auth/make-api {:conn conn :owner-emails owner-emails})))

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
;; Error Handling
;;=============================================================================

(defn- wrap-error-handler
  "Outermost middleware to catch all unhandled exceptions.
   Returns JSON error for /api routes, HTML for others."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (log/error "Unhandled exception:" (ex-message e) "\n" (with-out-str (.printStackTrace e)))
        (let [api-route? (clojure.string/starts-with? (or (:uri request) "") "/api")]
          (if api-route?
            (-> (resp/response "{\"error\":\"Internal server error\"}")
                (resp/status 500)
                (resp/content-type "application/json"))
            (-> (resp/response "<html><body><h1>500 - Server Error</h1><p>An unexpected error occurred.</p></body></html>")
                (resp/status 500)
                (resp/content-type "text/html"))))))))

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
   - GET /oauth2/google - Start Google OAuth flow
   - GET /oauth2/google/callback - OAuth callback
   - GET /logout - Clear session and redirect
   - GET /* - Static files from resources/public

   Options:
   - :google-client-id - Google OAuth client ID
   - :google-client-secret - Google OAuth client secret
   - :base-url - Base URL of the application
   - :allowed-email-pattern - Regex pattern for allowed emails (nil = all allowed)"
  ([api-fn] (make-app api-fn {}))
  ([api-fn {:keys [google-client-id google-client-secret base-url allowed-email-pattern]
            :or {base-url "http://localhost:8080"}}]
   (-> index-handler
       (wrap-resource "public")
       wrap-content-type
       (remote/wrap-api api-fn {:path "/api"})
       wrap-upload-route
       wrap-multipart-params
       wrap-keyword-params
       wrap-params
       oauth/wrap-logout
       (oauth/wrap-fetch-profile {:allowed-email-pattern allowed-email-pattern})
       (oauth/wrap-google-auth {:client-id google-client-id
                                :client-secret google-client-secret
                                :base-url base-url})
       (wrap-session {:store (memory-store)})
       wrap-error-handler)))

;;=============================================================================
;; Server Lifecycle
;;=============================================================================

(declare stop!)

(defn- get-env
  "Get environment variable with optional default."
  ([k] (get-env k nil))
  ([k default] (or (System/getenv k) default)))

(defn- parse-owner-emails
  "Parse comma-separated owner emails from environment variable."
  [s]
  (if (str/blank? s)
    #{}
    (set (map str/trim (str/split s #",")))))

(defn- parse-email-pattern
  "Parse email pattern from environment variable into a regex.
   Returns nil if blank (meaning all emails allowed)."
  [s]
  (when-not (str/blank? s)
    (re-pattern s)))

(defn start!
  "Start the blog server.

   Options:
   - :port - HTTP port (default 8080, or BLOG_PORT env var)
   - :seed? - Seed database with sample data (default true, ignored if :backup-dir)
   - :backup-dir - Directory to import posts from (optional, overrides :seed?)
   - :google-client-id - Google OAuth client ID (or GOOGLE_CLIENT_ID env var)
   - :google-client-secret - Google OAuth secret (or GOOGLE_CLIENT_SECRET env var)
   - :base-url - Application base URL (or BLOG_BASE_URL env var)
   - :owner-emails - Set of owner emails (or BLOG_OWNER_EMAILS env var, comma-separated)
   - :allowed-email-pattern - Regex pattern for allowed emails (or BLOG_ALLOWED_EMAILS env var)
                              Example: \".*@mycompany\\\\.com\" to allow only company emails"
  ([]
   (start! {}))
  ([opts]
   (let [{:keys [port seed? backup-dir google-client-id google-client-secret base-url owner-emails allowed-email-pattern]
          :or {port (parse-long (get-env "BLOG_PORT" "8080"))
               seed? true
               backup-dir (get-env "BLOG_BACKUP_DIR")
               google-client-id (get-env "GOOGLE_CLIENT_ID")
               google-client-secret (get-env "GOOGLE_CLIENT_SECRET")
               base-url (get-env "BLOG_BASE_URL" "http://localhost:8080")
               owner-emails (parse-owner-emails (get-env "BLOG_OWNER_EMAILS"))
               allowed-email-pattern (parse-email-pattern (get-env "BLOG_ALLOWED_EMAILS"))}} opts]
     (when @server
       (stop!))
     (log/info "Starting blog server...")
     (reset! conn (db/create-conn!))
     (log/log-db-connected db/default-cfg)
     (cond
       backup-dir
       (let [result (backup/import-all! @conn backup-dir)]
         (log/info "Imported" (:count result) "posts from" backup-dir))

       seed?
       (do (db/seed! @conn)
           (log/log-db-seeded 10)))
     (let [api-fn (make-api-fn @conn {:owner-emails owner-emails})
           app (make-app api-fn {:google-client-id google-client-id
                                 :google-client-secret google-client-secret
                                 :base-url base-url
                                 :allowed-email-pattern allowed-email-pattern})]
       (reset! server (http-kit/run-server app {:port port})))
     (log/log-startup port)
     (println (str "Blog server started on port " port))
     (println (str "  POST http://localhost:" port "/api - Pull endpoint"))
     (println (str "  GET  http://localhost:" port "/api/_schema - Schema"))
     (when google-client-id
       (println (str "  GET  http://localhost:" port "/oauth2/google - Google sign-in")))
     (when allowed-email-pattern
       (println (str "  Allowed emails: " allowed-email-pattern)))
     (when (seq owner-emails)
       (println (str "  Owners: " (str/join ", " owner-emails))))
     {:port port})))

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

  ;; Setup - use owner session for full CRUD access
  (def test-conn (db/create-conn!))
  (db/seed! test-conn)
  (def owner-emails #{"owner@test.com"})
  (def api-fn (make-api-fn test-conn {:owner-emails owner-emails}))
  (def app (make-app api-fn))

  (defn pull
    ([pattern] (pull pattern nil))
    ([pattern session]
     (let [req (cond-> (mock/request :post "/api")
                 true (mock/header "Content-Type" "application/transit+json")
                 true (mock/header "Accept" "application/transit+json")
                 true (mock/body (http/encode {:pattern pattern} :transit-json))
                 session (assoc :session session))
           resp (app req)]
       (http/decode (.readAllBytes (io/input-stream (:body resp))) :transit-json))))

  ;; Schema endpoint
  (:status (app (-> (mock/request :get "/api/_schema")
                    (mock/header "Accept" "application/transit+json")))) ;=> 200

  ;; LIST - response is {posts [...]} (works for all users)
  (count (get (pull '{:posts ?posts}) 'posts)) ;=> 10

  ;; CREATE - requires owner session
  (let [owner-session {:user-email "owner@test.com"}
        result (get (pull '{:posts {nil {:post/title "New" :post/content "---\nauthor: A\n---\n\nC"}}}
                          owner-session)
                    'posts)]
    [(:post/title result) (:post/author result)])
  ;=> ["New" "A"]

  ;; Cleanup
  (db/release-conn! test-conn)

  ;; Test wrap-error-handler returns 500 on exception
  (def error-handler (wrap-error-handler (fn [_] (throw (Exception. "test error")))))
  (:status (error-handler {:uri "/api/test"})) ;=> 500
  (:status (error-handler {:uri "/page"})) ;=> 500
  (get-in (error-handler {:uri "/api/test"}) [:headers "Content-Type"]) ;=> "application/json"
  (get-in (error-handler {:uri "/page"}) [:headers "Content-Type"])) ;=> "text/html"
