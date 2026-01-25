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
   on-success - fn of {:vars ...} where vars is symbol->value bindings
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
                   ;; Return both status and text for error handling
                   (-> (.text resp)
                       (.then (fn [text] {:ok (.-ok resp) :text text})))))
          (.then (fn [{:keys [ok text]}]
                   (let [response (decode text)]
                     (if (:errors response)
                       ;; Error response per spec
                       (on-error (or (-> response :errors first :reason)
                                     "Unknown error"))
                       ;; Success: response IS the variable bindings per spec
                       (on-success {:vars response})))))
          (.catch (fn [err]
                    (on-error (.-message err))))))
    (catch :default e
      (on-error (str "Parse error: " (.-message e))))))

(defn fetch-schema!
  "Fetch schema from remote server.

   url        - Server API endpoint (schema is at url/_schema)
   on-success - fn of schema map
   on-error   - fn of error string"
  [url on-success on-error]
  (let [schema-url (str url "/_schema")]
    (-> (js/fetch schema-url
                  #js {:method "GET"
                       :headers #js {"Accept" "application/transit+json"}})
        (.then (fn [resp]
                 (if (.-ok resp)
                   (.text resp)
                   (throw (js/Error. (str "HTTP " (.-status resp)))))))
        (.then (fn [text]
                 (on-success (decode text))))
        (.catch (fn [err]
                  (on-error (.-message err)))))))
