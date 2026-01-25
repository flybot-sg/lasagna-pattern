(ns sg.flybot.playground.server
  "Demo server with sample data for testing remote mode."
  (:require [org.httpkit.server :as http]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.params :refer [wrap-params]]
            [cognitect.transit :as t]
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

;;=============================================================================
;; API Handler
;;=============================================================================

(defn- handle-pull [body]
  (try
    (let [{:keys [pattern]} (transit-read body)
          matcher (impl/compile-pattern pattern)
          result (matcher (impl/vmr sample-data))]
      (if (impl/failure? result)
        {:status 400
         :body (transit-write {:error (str "Match failed: " (:reason result))})}
        {:status 200
         :body (transit-write (:val result))}))
    (catch Exception e
      {:status 400
       :body (transit-write {:error (str "Error: " (.getMessage e))})})))

(defn- api-handler [request]
  (case [(:request-method request) (:uri request)]
    [:post "/api"]
    (let [body (slurp (:body request))]
      (-> (handle-pull body)
          (assoc :headers {"Content-Type" "application/transit+json"})))

    [:get "/health"]
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body "OK"}

    {:status 404
     :headers {"Content-Type" "text/plain"}
     :body "Not found"}))

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
