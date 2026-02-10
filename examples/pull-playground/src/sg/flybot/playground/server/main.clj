(ns sg.flybot.playground.server.main
  "Demo server for testing remote mode.
   Uses standard Remote Pull Protocol handler.
   Schema and reset are pull-able data, not separate endpoints."
  (:require [org.httpkit.server :as http]
            [ring.middleware.params :refer [wrap-params]]
            [sg.flybot.playground.common.data :as data]
            [sg.flybot.pullable.remote :as remote]
            [sg.flybot.pullable.collection :as coll]
            [sg.flybot.pullable.sample :as sample]
            [sg.flybot.pullable.malli]
            [malli.core :as m]))

;;=============================================================================
;; Middleware
;;=============================================================================

(defn- wrap-cors
  "Add CORS headers for cross-origin requests."
  [handler]
  (fn [request]
    (let [origin (get-in request [:headers "origin"] "*")]
      (if (= :options (:request-method request))
        {:status 204
         :headers {"Access-Control-Allow-Origin" origin
                   "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
                   "Access-Control-Allow-Headers" "Content-Type, Accept"
                   "Access-Control-Max-Age" "86400"}}
        (-> (handler request)
            (assoc-in [:headers "Access-Control-Allow-Origin"] origin))))))

;;=============================================================================
;; Sources & Collections
;;=============================================================================

(defn- make-sources [raw-data]
  {:users (coll/atom-source {:initial (:users raw-data)})
   :posts (coll/atom-source {:initial (:posts raw-data)})})

(defn- make-data
  "Build the data map for api-fn.
   Includes collections, schema as pull-able data, and reset as a Mutable."
  [system-atom sources schema sample]
  (let [colls {:users  (coll/collection (:users sources))
               :posts  (coll/collection (:posts sources))
               :config (:config data/default-data)}]
    (assoc colls
           :schema {:schema schema :sample sample}
           :seed  (reify
                    coll/Mutable
                    (mutate! [_ _query _value]
                      (let [new-srcs (make-sources data/default-data)]
                        (swap! system-atom assoc
                               :sources new-srcs
                               :data (make-data system-atom new-srcs schema sample))
                        true))
                    coll/Wireable
                    (->wire [_] nil)))))

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

(defn- make-app [{:keys [system-atom schema sample]}]
  (let [api-fn       (fn [_ring-request]
                       {:data   (:data @system-atom)
                        :schema schema
                        :sample sample})
        pull-handler (remote/make-handler api-fn)]
    (-> (fn [request]
          (or (health-handler request)
              (pull-handler request)))
        wrap-params
        wrap-cors)))

;;=============================================================================
;; System Lifecycle
;;=============================================================================

(defonce system (atom nil))

(defn start! [& [{:keys [port] :or {port 8081}}]]
  (let [sources    (make-sources data/default-data)
        schema     (m/schema data/default-schema)
        api-schema (m/schema (conj data/default-schema
                                   [:schema {:optional true} :any]
                                   [:seed {:optional true} :any]))
        sample     (sample/generate schema {:size 10 :seed 42 :min 5})]
    (reset! system {:sources sources
                    :data    (make-data system sources schema sample)
                    :schema  api-schema})
    (let [app     (make-app {:system-atom system :schema api-schema :sample sample})
          stop-fn (http/run-server app {:port port})]
      (swap! system assoc :stop-fn stop-fn)
      (println (str "Server running at http://localhost:" port)))))

(defn stop! []
  (when-let [{:keys [stop-fn]} @system]
    (stop-fn :timeout 100)
    (reset! system nil)
    (println "Server stopped")))

(defn -main [& args]
  (let [port (if (seq args) (Integer/parseInt (first args)) 8081)]
    (start! {:port port})
    @(promise)))
