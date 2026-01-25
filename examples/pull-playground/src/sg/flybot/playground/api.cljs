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

(defn- format-error
  "Format an error response to a user-friendly string."
  [errors]
  (let [{:keys [code reason path]} (first errors)]
    (str (name code) ": " reason
         (when (seq path)
           (str " at " (pr-str path))))))

(defn pull!
  "Execute a pull query against a remote server.

   url        - Server API endpoint
   pattern-str - Pattern as EDN string
   on-success - fn of bindings map (symbol -> value)
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
                   (let [response (decode text)]
                     ;; Check for error response
                     (if (:errors response)
                       (on-error (format-error (:errors response)))
                       ;; Success: response is directly the bindings map
                       (on-success response)))))
          (.catch (fn [err]
                    (on-error (.-message err))))))
    (catch :default e
      (on-error (str "Parse error: " (.-message e))))))
