(ns sg.flybot.blog.ui.api
  "API client - browser-only, uses fetch."
  (:require [cognitect.transit :as t]))

(def ^:private api-url "/api")

(defn- transit-reader []
  (t/reader :json))

(defn- transit-writer []
  (t/writer :json))

(defn- encode [data]
  (t/write (transit-writer) data))

(defn- decode [s]
  (t/read (transit-reader) s))

(defn pull!
  "Execute a pull query against the API.

   pattern - Pull pattern like '{:posts ?posts}
   on-success - fn of response data
   on-error - fn of error"
  [pattern on-success on-error]
  (-> (js/fetch api-url
                #js {:method "POST"
                     :headers #js {"Content-Type" "application/transit+json"
                                   "Accept" "application/transit+json"}
                     :body (encode {:pattern pattern})})
      (.then (fn [resp]
               (if (.-ok resp)
                 (.text resp)
                 (throw (js/Error. (str "HTTP " (.-status resp)))))))
      (.then (fn [text]
               (on-success (decode text))))
      (.catch on-error)))
