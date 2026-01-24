(ns sg.flybot.pullable.remote.http
  "HTTP transport implementation for pull protocol.

   Internal namespace - use sg.flybot.pullable.remote for public API."
  (:require
   [clojure.string :as str]
   [sg.flybot.pullable.impl :as pattern]
   #?(:clj [cognitect.transit :as transit])
   #?(:clj [clojure.edn :as edn])
   #?(:clj [clojure.java.io :as io]))
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream])))

;;=============================================================================
;; Content Types & Negotiation
;;=============================================================================

(def content-types
  {:transit-json    "application/transit+json"
   :transit-msgpack "application/transit+msgpack"
   :edn             "application/edn"})

(def ^:private content-type->format
  (into {} (map (fn [[k v]] [v k]) content-types)))

(defn- parse-content-type
  "Parse content-type header into format keyword."
  [content-type]
  (some-> content-type
          (str/split #";")
          first
          str/trim
          str/lower-case
          content-type->format))

(defn- negotiate-format
  "Select response format from Accept header. Defaults to :transit-json."
  [accept-header]
  (if (or (nil? accept-header) (= "*/*" accept-header))
    :transit-json
    (or (->> (str/split accept-header #",")
             (map str/trim)
             (some #(some (fn [[fmt ct]] (when (str/starts-with? % ct) fmt))
                          content-types)))
        :transit-json)))

;;=============================================================================
;; Encoding
;;=============================================================================

#?(:clj
   (defn- transit-write [value format]
     (let [out (ByteArrayOutputStream.)
           writer (transit/writer out format)]
       (transit/write writer value)
       (.toByteArray out))))

#?(:clj
   (defn- transit-read [input format]
     (let [in (if (instance? ByteArrayInputStream input)
                input
                (ByteArrayInputStream. input))]
       (transit/read (transit/reader in format)))))

#?(:clj
   (defn- edn-write [value]
     (.getBytes (pr-str value) "UTF-8")))

#?(:clj
   (defn- edn-read [input]
     (let [s (if (instance? ByteArrayInputStream input)
               (slurp input)
               (String. ^bytes input "UTF-8"))]
       (edn/read-string {:readers {'? #(list '? %)}} s))))

(defn encode
  "Encode value to bytes. Format: :transit-json, :transit-msgpack, or :edn."
  [value format]
  #?(:clj
     (case format
       :transit-json    (transit-write value :json)
       :transit-msgpack (transit-write value :msgpack)
       :edn             (edn-write value))
     :cljs
     (throw (ex-info "Server-side only" {:format format}))))

(defn decode
  "Decode bytes to Clojure data."
  [input format]
  #?(:clj
     (case format
       :transit-json    (transit-read input :json)
       :transit-msgpack (transit-read input :msgpack)
       :edn             (edn-read input))
     :cljs
     (throw (ex-info "Server-side only" {:format format}))))

;;=============================================================================
;; Pattern Parameterization
;;=============================================================================

(defn- param-rule
  "Create a rule that substitutes a $-prefixed symbol with a value.
   Follows rule protocol: returns value on match, nil otherwise."
  [param-sym value]
  (fn [data]
    (when (= data param-sym)
      value)))

(defn- params->rules
  "Convert params map to substitution rules.
   {:id \"123\"} -> rule replacing '$id with \"123\""
  [params]
  (map (fn [[k v]]
         (param-rule (symbol (str "$" (name k))) v))
       params))

(defn- resolve-params
  "Apply parameter substitution to pattern using rule system."
  [pattern params]
  (if (seq params)
    (pattern/apply-rules (params->rules params) pattern)
    pattern))

^:rct/test
(comment
  ;; param-rule: matches exact symbol
  ((param-rule '$id "123") '$id) ;=> "123"
  ;; param-rule: returns nil on no match
  ((param-rule '$id "123") '$other) ;=> nil
  ((param-rule '$id "123") :id) ;=> nil

  ;; resolve-params: substitutes $-prefixed symbols
  (resolve-params '{:user {:id $id :name ?name}} {:id "user-123"})
  ;=> '{:user {:id "user-123" :name ?name}}

  ;; resolve-params: multiple params
  (resolve-params '{:user {:id $id :role $role}} {:id "123" :role "admin"})
  ;=> '{:user {:id "123" :role "admin"}}

  ;; resolve-params: nested structures
  (resolve-params '{:query {:filter {:status $status}}} {:status "active"})
  ;=> '{:query {:filter {:status "active"}}}

  ;; resolve-params: structural params (maps/vectors as values)
  (resolve-params '{:query $filter} {:filter {:status "active" :role "admin"}})
  ;=> '{:query {:status "active" :role "admin"}}

  ;; resolve-params: empty params returns pattern unchanged
  (resolve-params '{:user {:id $id}} {})
  ;=> '{:user {:id $id}}
  (resolve-params '{:user {:id $id}} nil)
  ;=> '{:user {:id $id}}
  )

;;=============================================================================
;; Response Helpers
;;=============================================================================

(defn- success [data vars]
  {:data data :vars vars})

(defn- error [code reason & [path]]
  (cond-> {:code code :reason reason}
    path (assoc :path path)))

(defn- failure [err]
  {:errors (if (sequential? err) err [err])})

(defn- success? [response]
  (and (contains? response :data)
       (not (contains? response :errors))))

(defn- match-failure->error
  "Convert pattern MatchFailure to response error."
  [{:keys [reason matcher-type path value]}]
  (cond-> {:code (case matcher-type
                   :schema :schema-violation
                   :bind   :binding-conflict
                   :match-failure)
           :reason reason}
    (seq path) (assoc :path path)
    value (assoc :value value)))

;;=============================================================================
;; Ring Handler Implementation
;;=============================================================================

(defn- read-body [body]
  #?(:clj
     (cond
       (nil? body) nil
       (bytes? body) body
       :else (with-open [in (io/input-stream body)]
               (.readAllBytes in)))
     :cljs body))

(defn- ring-response [status body-bytes content-type]
  {:status status
   :headers {"Content-Type" content-type}
   :body #?(:clj (ByteArrayInputStream. body-bytes) :cljs body-bytes)})

(defn- encode-response [response format]
  {:body (encode response format)
   :content-type (get content-types format)})

(defn- execute-pull
  "Execute pull pattern against API data.
   Params in pull-request are used for:
   1. Pattern parameterization: $key in pattern replaced with value
   2. Ring request params: merged into ring-request :params"
  [api-fn ring-request pull-request]
  (try
    (let [params (:params pull-request)
          ;; Resolve $params in pattern using rule-based substitution
          resolved-pattern (resolve-params (:pattern pull-request) params)
          ;; Also merge params into ring request for API use
          ring-request (update ring-request :params merge params)
          {:keys [data schema]} (api-fn ring-request)
          compiled (pattern/compile-pattern
                    resolved-pattern
                    (cond-> {} schema (assoc :schema schema)))
          result (compiled (pattern/vmr data))]
      (if (pattern/failure? result)
        (failure (match-failure->error result))
        (success (:val result) (:vars result))))
    (catch #?(:clj Exception :cljs js/Error) e
      (failure (error :execution-error
                      #?(:clj (.getMessage e) :cljs (.-message e)))))))

(defn- handle-pull [api-fn ring-request]
  (let [req-fmt (or (parse-content-type (get-in ring-request [:headers "content-type"]))
                    :transit-json)
        res-fmt (negotiate-format (get-in ring-request [:headers "accept"]))
        body (read-body (:body ring-request))
        response (cond
                   (nil? body)
                   (failure (error :invalid-request "Request body required"))

                   :else
                   (try
                     (let [req (decode body req-fmt)]
                       (if (and (map? req) (contains? req :pattern))
                         (execute-pull api-fn ring-request req)
                         (failure (error :invalid-request "Request must contain :pattern"))))
                     (catch #?(:clj Exception :cljs js/Error) e
                       (failure (error :decode-error
                                       (str "Failed to decode: "
                                            #?(:clj (.getMessage e) :cljs (.-message e))))))))
        {:keys [body content-type]} (encode-response response res-fmt)]
    (ring-response (if (success? response) 200 400) body content-type)))

(defn- handle-schema [api-fn ring-request]
  (let [res-fmt (negotiate-format (get-in ring-request [:headers "accept"]))
        schema (:schema (api-fn ring-request))
        response (if schema
                   (success schema {})
                   (failure (error :not-found "No schema available")))
        {:keys [body content-type]} (encode-response response res-fmt)]
    (ring-response (if (success? response) 200 404) body content-type)))

(defn- handle-not-found [ring-request]
  (let [res-fmt (negotiate-format (get-in ring-request [:headers "accept"]))
        {:keys [body content-type]} (encode-response
                                     (failure (error :not-found "Not found"))
                                     res-fmt)]
    (ring-response 404 body content-type)))

(defn- handle-method-not-allowed [allowed ring-request]
  (let [res-fmt (negotiate-format (get-in ring-request [:headers "accept"]))
        {:keys [body content-type]} (encode-response
                                     (failure (error :method-not-allowed
                                                     (str "Allowed: " allowed)))
                                     res-fmt)]
    (-> (ring-response 405 body content-type)
        (assoc-in [:headers "Allow"] allowed))))

;;=============================================================================
;; Public Handler Constructor
;;=============================================================================

(defn make-handler
  "Create Ring handler for pull API.

   api-fn: (ring-request) → {:data lazy-map, :schema schema-map}

   Options:
   - :path - Base path (default \"/api\")"
  ([api-fn] (make-handler api-fn {}))
  ([api-fn {:keys [path] :or {path "/api"}}]
   (let [schema-path (str path "/_schema")]
     (fn [request]
       (let [uri (:uri request)
             method (:request-method request)]
         (cond
           (and (= uri path) (= method :post))
           (handle-pull api-fn request)

           (and (= uri schema-path) (= method :get))
           (handle-schema api-fn request)

           (= uri path)
           (handle-method-not-allowed "POST" request)

           (= uri schema-path)
           (handle-method-not-allowed "GET" request)

           :else
           (handle-not-found request)))))))
