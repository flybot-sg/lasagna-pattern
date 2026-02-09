(ns sg.flybot.playground.server.main
  "Demo server for testing remote mode.
   Uses standard Remote Pull Protocol v0.2 handler."
  (:require [org.httpkit.server :as http]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.params :refer [wrap-params]]
            [sg.flybot.playground.common.data :as data]
            [sg.flybot.pullable.remote :as remote]
            [sg.flybot.pullable.collection :as coll]
            [sg.flybot.pullable.sample :as sample]
            [sg.flybot.pullable.malli]
            [malli.core :as m]))

;;=============================================================================
;; Sample Data (from shared data.cljc)
;;=============================================================================

(def users-source
  (coll/atom-source {:initial (:users data/default-data)}))

(def posts-source
  (coll/atom-source {:initial (:posts data/default-data)}))

(def sample-data
  {:users  (coll/collection users-source)
   :posts  (coll/collection posts-source)
   :config (:config data/default-data)})

(def sample-schema
  (m/schema data/default-schema))

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
