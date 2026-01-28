(ns sg.flybot.playground.server
  "Demo server with sample data for testing remote mode.
   Uses standard Remote Pull Protocol v0.2 handler."
  (:require [org.httpkit.server :as http]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.params :refer [wrap-params]]
            [sg.flybot.pullable.remote :as remote]
            [sg.flybot.pullable.collection :as coll]
            [sg.flybot.pullable.sample :as sample]
            [sg.flybot.pullable.malli]
            [malli.core :as m]))

;;=============================================================================
;; Sample Data
;;=============================================================================

(def users-source
  (coll/atom-source
   {:initial [{:id 1 :name "Alice" :email "alice@example.com" :role :admin}
              {:id 2 :name "Bob" :email "bob@example.com" :role :user}
              {:id 3 :name "Carol" :email "carol@example.com" :role :user}]}))

(def posts-source
  (coll/atom-source
   {:initial [{:id 1 :title "Hello World" :author "Alice" :tags ["intro" "welcome"]}
              {:id 2 :title "Pattern Matching" :author "Bob" :tags ["tutorial" "patterns"]}
              {:id 3 :title "Advanced Topics" :author "Alice" :tags ["advanced"]}]}))

(def sample-data
  {:users  (coll/collection users-source)
   :posts  (coll/collection posts-source)
   :config {:version "1.0.0"
            :features {:dark-mode true :notifications false}}})

(def sample-schema
  "Schema describing the sample data structure with inline Malli documentation.
   Uses Malli's hiccup syntax with properties on each field entry."
  (m/schema
   [:map {:version "1.0.0"
          :doc "Sample API for Pull Pattern Playground"}
    [:users {:doc "User accounts"}
     [:vector {:ilookup true}
      [:map
       [:id {:doc "Unique identifier" :example 1} :int]
       [:name {:doc "Display name" :example "Alice"} :string]
       [:email {:doc "Email address" :example "alice@example.com"} :string]
       [:role {:doc "User role" :example :admin} :keyword]]]]
    [:posts {:doc "Blog posts"
             :operations {:list "Returns all posts"
                          :get  "Lookup by {:id n}"}}
     [:vector {:ilookup true}
      [:map
       [:id {:doc "Post identifier" :example 1} :int]
       [:title {:doc "Post title" :example "Hello World"} :string]
       [:author {:doc "Author name" :example "Alice"} :string]
       [:tags {:doc "Post tags" :example ["intro" "welcome"]} [:vector :string]]]]]
    [:config {:doc "Application configuration"}
     [:map
      [:version {:doc "App version"} :string]
      [:features {:doc "Feature flags"}
       [:map
        [:dark-mode {:doc "Dark mode enabled"} :boolean]
        [:notifications {:doc "Notifications enabled"} :boolean]]]]]]))

;;=============================================================================
;; API Function
;;=============================================================================

(defn api-fn
  "API function for Remote Pull Protocol.
   Returns data and schema for pattern matching."
  [_ring-request]
  {:data   sample-data
   :schema sample-schema
   :sample (sample/generate sample-schema {:size 10 :seed 42 :min 5})})

(defn- health-handler
  "Health check endpoint."
  [request]
  (when (and (= :get (:request-method request))
             (= "/health" (:uri request)))
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body "OK"}))

;;=============================================================================
;; Middleware
;;=============================================================================

(def app
  (let [pull-handler (remote/make-handler api-fn)]
    (-> (fn [request]
          (or (health-handler request)
              (pull-handler request)))
        wrap-params
        (wrap-cors :access-control-allow-origin [#".*"]
                   :access-control-allow-methods [:get :post :options]
                   :access-control-allow-headers ["Content-Type" "Accept"]))))

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
