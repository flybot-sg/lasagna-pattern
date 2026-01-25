(ns sg.flybot.playground.server
  "Demo server with sample data for testing remote mode.
   Implements Remote Pull Protocol v0.2 specification."
  (:require [org.httpkit.server :as http]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.params :refer [wrap-params]]
            [cognitect.transit :as t]
            [clojure.string :as str]
            [sg.flybot.pullable.impl :as impl])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

;;=============================================================================
;; Transit Encoding/Decoding
;;=============================================================================

(defn- transit-read [s]
  (let [in (ByteArrayInputStream. (.getBytes s "UTF-8"))
        reader (t/reader in :json)]
    (t/read reader)))

(defn- transit-write [data]
  (let [out (ByteArrayOutputStream.)
        writer (t/writer out :json)]
    (t/write writer data)
    (.toString out "UTF-8")))

;;=============================================================================
;; Sample Data
;;=============================================================================

(def sample-data
  {:users [{:id 1 :name "Alice" :email "alice@example.com" :role :admin}
           {:id 2 :name "Bob" :email "bob@example.com" :role :user}
           {:id 3 :name "Carol" :email "carol@example.com" :role :user}]
   :posts [{:id 1 :title "Hello World" :author "Alice" :tags ["intro" "welcome"]}
           {:id 2 :title "Pattern Matching" :author "Bob" :tags ["tutorial" "patterns"]}
           {:id 3 :title "Advanced Topics" :author "Alice" :tags ["advanced"]}]
   :config {:version "1.0.0"
            :features {:dark-mode true :notifications false}}})

(def sample-schema
  "Schema describing the sample data structure with documentation metadata.
   Per spec section 7.3, uses :doc, :fields, :version, :example, :deprecated."
  ^{:version "1.0.0"
    :doc "Sample API for Pull Pattern Playground"
    :fields {:users {:doc "User accounts"
                     :fields {:id    {:doc "Unique identifier" :example 1}
                              :name  {:doc "Display name" :example "Alice"}
                              :email {:doc "Email address" :example "alice@example.com"}
                              :role  {:doc "User role" :example :admin}}}
             :posts {:doc "Blog posts"
                     :operations {:list "Returns all posts"
                                  :get  "Lookup by {:id n}"}
                     :fields {:id     {:doc "Post identifier" :example 1}
                              :title  {:doc "Post title" :example "Hello World"}
                              :author {:doc "Author name" :example "Alice"}
                              :tags   {:doc "Post tags" :example ["intro" "welcome"]}}}
             :config {:doc "Application configuration"
                      :fields {:version  {:doc "App version"}
                               :features {:doc "Feature flags"
                                          :fields {:dark-mode     {:doc "Dark mode enabled"}
                                                   :notifications {:doc "Notifications enabled"}}}}}}}
  {:users [{:id :number
            :name :string
            :email :string
            :role :keyword}]
   :posts [{:id :number
            :title :string
            :author :string
            :tags [:string]}]
   :config {:version :string
            :features {:dark-mode :boolean
                       :notifications :boolean}}})

;;=============================================================================
;; API Handler
;;=============================================================================

(defn- handle-pull [body]
  (try
    (let [{:keys [pattern]} (transit-read body)
          matcher (impl/compile-pattern pattern)
          result (matcher (impl/vmr sample-data))]
      (if (impl/failure? result)
        ;; Per spec: match-failure → 422 Unprocessable Content
        {:status 422
         :body (transit-write {:errors [{:code :match-failure
                                         :reason (:reason result)
                                         :path (:path result)}]})}
        {:status 200
         ;; Per spec: success response returns variable bindings directly
         :body (transit-write (:vars result))}))
    (catch Exception e
      (let [msg (.getMessage e)]
        (if (or (re-find #"decode" (str/lower-case (or msg "")))
                (re-find #"parse" (str/lower-case (or msg ""))))
          ;; Per spec: decode-error → 400
          {:status 400
           :body (transit-write {:errors [{:code :decode-error
                                           :reason msg}]})}
          ;; Per spec: execution-error → 500
          {:status 500
           :body (transit-write {:errors [{:code :execution-error
                                           :reason msg}]})})))))

(defn- api-handler [request]
  (let [method (:request-method request)
        uri (:uri request)]
    (cond
      ;; POST /api - pattern execution
      (and (= method :post) (= uri "/api"))
      (let [body (slurp (:body request))]
        (-> (handle-pull body)
            (assoc :headers {"Content-Type" "application/transit+json"})))

      ;; GET /api/_schema - introspection
      ;; NOTE: Transit doesn't preserve metadata by default.
      ;; Per spec 7.3, schema documentation lives in metadata.
      ;; Production servers should use custom Transit write handlers
      ;; to serialize metadata. This demo returns the schema structure only.
      (and (= method :get) (= uri "/api/_schema"))
      {:status 200
       :headers {"Content-Type" "application/transit+json"}
       :body (transit-write sample-schema)}

      ;; GET /health - health check
      (and (= method :get) (= uri "/health"))
      {:status 200
       :headers {"Content-Type" "text/plain"}
       :body "OK"}

      ;; Wrong method on /api - per spec: 405 with Allow header
      (= uri "/api")
      {:status 405
       :headers {"Content-Type" "application/transit+json"
                 "Allow" "POST"}
       :body (transit-write {:errors [{:code :method-not-allowed
                                       :reason (str "Method " (name method) " not allowed on /api")}]})}

      ;; Wrong method on /api/_schema - per spec: 405 with Allow header
      (= uri "/api/_schema")
      {:status 405
       :headers {"Content-Type" "application/transit+json"
                 "Allow" "GET"}
       :body (transit-write {:errors [{:code :method-not-allowed
                                       :reason (str "Method " (name method) " not allowed on /api/_schema")}]})}

      ;; Not found - per spec: 404
      :else
      {:status 404
       :headers {"Content-Type" "application/transit+json"}
       :body (transit-write {:errors [{:code :not-found
                                       :reason (str "Endpoint " uri " not found")}]})})))

;;=============================================================================
;; Middleware
;;=============================================================================

(def app
  (-> api-handler
      wrap-params
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :options]
                 :access-control-allow-headers ["Content-Type" "Accept"])))

;;=============================================================================
;; Server
;;=============================================================================

(defonce server (atom nil))

(defn start! [& [{:keys [port] :or {port 8081}}]]
  (println (str "Starting demo server on port " port "..."))
  (println "Sample data available:")
  (println "  :users - List of users")
  (println "  :posts - List of posts")
  (println "  :config - App configuration")
  (reset! server (http/run-server app {:port port}))
  (println (str "Server running at http://localhost:" port)))

(defn stop! []
  (when-let [s @server]
    (s :timeout 100)
    (reset! server nil)
    (println "Server stopped")))

(defn -main [& args]
  (let [port (if (seq args) (Integer/parseInt (first args)) 8081)]
    (start! {:port port})
    @(promise))) ; Keep running
