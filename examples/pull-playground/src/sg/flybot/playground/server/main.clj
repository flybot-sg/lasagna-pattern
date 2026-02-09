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
;; Handlers
;;=============================================================================

(defn- health-handler
  "Health check endpoint."
  [request]
  (when (and (= :get (:request-method request))
             (= "/health" (:uri request)))
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body "OK"}))

(defn- make-app [{:keys [data schema]}]
  (let [api-fn       (fn [_ring-request]
                       {:data   data
                        :schema schema
                        :sample (sample/generate schema {:size 10 :seed 42 :min 5})})
        pull-handler (remote/make-handler api-fn)]
    (-> (fn [request]
          (or (health-handler request)
              (pull-handler request)))
        wrap-params
        (wrap-cors :access-control-allow-origin [#".*"]
                   :access-control-allow-methods [:get :post :options]
                   :access-control-allow-headers ["Content-Type" "Accept"]))))

;;=============================================================================
;; System Lifecycle
;;=============================================================================

(defonce system (atom nil))

(defn start! [& [{:keys [port] :or {port 8081}}]]
  (let [sources {:users (coll/atom-source {:initial (:users data/default-data)})
                 :posts (coll/atom-source {:initial (:posts data/default-data)})}
        colls   {:users  (coll/collection (:users sources))
                 :posts  (coll/collection (:posts sources))
                 :config (:config data/default-data)}
        schema  (m/schema data/default-schema)
        app     (make-app {:data colls :schema schema})
        stop-fn (http/run-server app {:port port})]
    (reset! system {:stop-fn stop-fn :data colls :schema schema})
    (println (str "Server running at http://localhost:" port))))

(defn stop! []
  (when-let [{:keys [stop-fn]} @system]
    (stop-fn :timeout 100)
    (reset! system nil)
    (println "Server stopped")))

(defn -main [& args]
  (let [port (if (seq args) (Integer/parseInt (first args)) 8081)]
    (start! {:port port})
    @(promise)))
