(ns sg.flybot.flybot-site.ui.api
  "API client - browser-only, uses fetch."
  (:require [cognitect.transit :as t]
            [sg.flybot.flybot-site.ui.log :as log]))

(def ^:private api-url "/api")

(defn- transit-reader []
  (t/reader :json))

(defn- transit-writer []
  (t/writer :json))

(defn- encode [data]
  (t/write (transit-writer) data))

(defn- decode [s]
  (t/read (transit-reader) s))

(defn- error->string
  "Convert error to string for classification."
  [err]
  (cond
    (instance? js/Error err) (.-message err)
    (string? err) err
    :else (str err)))

(defn pull!
  "Execute a pull query against the API.

   pattern - Pull pattern like '{:posts ?posts}
   on-success - fn of response data
   on-error - fn of error map {:code :forbidden :reason \"...\" :status 403} or string"
  [pattern on-success on-error]
  (log/log-api-request pattern)
  (-> (js/fetch api-url
                #js {:method "POST"
                     :headers #js {"Content-Type" "application/transit+json"
                                   "Accept" "application/transit+json"}
                     :credentials "include"
                     :body (encode {:pattern pattern})})
      (.then (fn [resp]
               ;; Always read body - errors have structured data
               (-> (.text resp)
                   (.then (fn [text]
                            {:status (.-status resp)
                             :ok (.-ok resp)
                             :body (when (seq text) (decode text))})))))
      (.then (fn [{:keys [status ok body]}]
               (if ok
                 (do (log/log-api-response body)
                     (on-success body))
                 ;; Extract structured error from response
                 (let [error (if-let [err (first (:errors body))]
                               (assoc err :status status)
                               {:code :unknown :reason (str "HTTP " status) :status status})]
                   (log/log-api-error error pattern)
                   (on-error error)))))
      (.catch (fn [err]
                (let [msg (error->string err)]
                  (log/log-api-error msg pattern)
                  (on-error {:code :network :reason msg :status 0}))))))

(defn upload-image!
  "Upload image blob to server. Returns promise resolving to image URL."
  [blob]
  (log/debug "Uploading image:" (.-name blob) "size:" (.-size blob))
  (let [form-data (js/FormData.)]
    (.append form-data "image" blob (.-name blob))
    (-> (js/fetch "/api/upload" #js {:method "POST" :body form-data})
        (.then (fn [resp]
                 (if (.-ok resp)
                   (.json resp)
                   (throw (js/Error. (str "HTTP " (.-status resp)))))))
        (.then (fn [json]
                 (if-let [url (.-url json)]
                   (do (log/info "Image uploaded:" url) url)
                   (throw (js/Error. (or (.-error json) "Upload failed"))))))
        (.catch (fn [err]
                  (log/error "Image upload failed:" (error->string err))
                  (throw err))))))
