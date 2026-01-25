(ns sg.flybot.playground.api
  "Remote API client using Transit."
  (:require [cognitect.transit :as t]
            [cljs.reader :as reader]))

(defn- transit-reader []
  (t/reader :json))

(defn- transit-writer []
  (t/writer :json))

(defn- encode [data]
  (t/write (transit-writer) data))

(defn- decode [s]
  (t/read (transit-reader) s))

(defn pull!
  "Execute a pull query against a remote server.

   url        - Server API endpoint
   pattern-str - Pattern as EDN string
   on-success - fn of {:data ... :vars ...}
   on-error   - fn of error string"
  [url pattern-str on-success on-error]
  (try
    (let [pattern (reader/read-string pattern-str)]
      (-> (js/fetch url
                    #js {:method "POST"
                         :headers #js {"Content-Type" "application/transit+json"
                                       "Accept" "application/transit+json"}
                         :body (encode {:pattern pattern})})
          (.then (fn [resp]
                   (if (.-ok resp)
                     (.text resp)
                     (throw (js/Error. (str "HTTP " (.-status resp)))))))
          (.then (fn [text]
                   (let [response (decode text)
                         ;; Extract result from response (handles symbol keys)
                         result-key (some #(when (symbol? %) %) (keys response))
                         result (get response result-key response)]
                     (on-success {:data result :vars {}}))))
          (.catch (fn [err]
                    (on-error (.-message err))))))
    (catch :default e
      (on-error (str "Parse error: " (.-message e))))))
