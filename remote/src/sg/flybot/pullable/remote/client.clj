(ns sg.flybot.pullable.remote.client
  "HTTP client for pull-based APIs.

   Provides a simple way to interact with remote pull APIs from the REPL.

   ```clojure
   (def api (connect \"http://localhost:8080/api\"))

   ;; Pull data
   (api '{:posts ?posts})

   ;; With params
   (api '{:post {:title ?t}} {:post-id 1})

   ;; Introspect schema
   (schema api)
   ```"
  (:require
   [sg.flybot.pullable.remote.http :as http])
  (:import
   [java.net URI]
   [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
   [java.time Duration]))

;;=============================================================================
;; HTTP Client
;;=============================================================================

(defonce ^:private default-client
  (delay
    (-> (HttpClient/newBuilder)
        (.connectTimeout (Duration/ofSeconds 10))
        (.build))))

(defn- make-request
  "Build an HttpRequest."
  [method uri content-type body]
  (let [builder (-> (HttpRequest/newBuilder)
                    (.uri (URI/create uri))
                    (.timeout (Duration/ofSeconds 30))
                    (.header "Accept" content-type))]
    (case method
      :get (.GET builder)
      :post (-> builder
                (.header "Content-Type" content-type)
                (.POST (HttpRequest$BodyPublishers/ofByteArray body))))
    (.build builder)))

(defn- send-request
  "Send HTTP request and return response body bytes."
  [^HttpClient client request]
  (let [response (.send client request (HttpResponse$BodyHandlers/ofByteArray))]
    {:status (.statusCode response)
     :body (.body response)}))

;;=============================================================================
;; Public API
;;=============================================================================

(defn connect
  "Create a connection to a pull API endpoint.

   Returns a function that executes pull patterns:
     (def api (connect \"http://localhost:8080/api\"))
     (api '{:posts ?posts})
     (api '{:post {:title ?t}} {:post-id 1})

   Options:
   - :format - Wire format (:transit-json, :transit-msgpack, :edn). Default :transit-json
   - :client - Custom HttpClient instance"
  ([url]
   (connect url {}))
  ([url {:keys [format client] :or {format :transit-json}}]
   (let [client (or client @default-client)
         content-type (get http/content-types format)
         schema-url (str url "/_schema")]
     (with-meta
       (fn pull
         ([pattern]
          (pull pattern {}))
         ([pattern params]
          (let [request-body (http/encode {:pattern pattern :params params} format)
                request (make-request :post url content-type request-body)
                {:keys [status body]} (send-request client request)
                response (http/decode body format)]
            (if (< status 400)
              response
              (throw (ex-info "Pull request failed"
                              {:status status :response response}))))))
       {:url url
        :schema-url schema-url
        :format format
        :client client}))))

(defn schema
  "Fetch schema from a connected API.

   (schema api)"
  [api-fn]
  (let [{:keys [schema-url format client]} (meta api-fn)
        content-type (get http/content-types format)
        request (make-request :get schema-url content-type nil)
        {:keys [status body]} (send-request client request)
        response (http/decode body format)]
    (if (< status 400)
      (:data response)
      (throw (ex-info "Schema request failed"
                      {:status status :response response})))))

(defn url
  "Get the URL of a connected API."
  [api-fn]
  (:url (meta api-fn)))

^:rct/test
(comment
  ;; These tests require a running server
  ;; See examples/blog for a working example

  connect ;=>> fn?
  schema ;=>> fn?
  url) ;=>> fn?)
