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

(defn pull!
  "Execute a pull query against the API.

   pattern - Pull pattern like '{:posts ?posts}
   on-success - fn of response data
   on-error - fn of error"
  [pattern on-success on-error]
  (log/log-api-request pattern)
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
               (let [response (decode text)]
                 (log/log-api-response response)
                 (on-success response))))
      (.catch (fn [err]
                (log/log-api-error err pattern)
                (on-error err)))))

(defn upload-image!
  "Upload image blob to server. Returns promise resolving to image URL."
  [blob]
  (log/debug "Uploading image:" (.-name blob) "size:" (.-size blob))
  (let [form-data (js/FormData.)]
    (.append form-data "image" blob (.-name blob))
    (-> (js/fetch "/api/upload" #js {:method "POST" :body form-data})
        (.then #(.json %))
        (.then (fn [json]
                 (let [url (.-url json)]
                   (log/info "Image uploaded:" url)
                   url)))
        (.catch (fn [err]
                  (log/error "Image upload failed:" err)
                  (throw err))))))
