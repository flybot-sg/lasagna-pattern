(ns ^:no-doc sg.flybot.pullable.remote.http
  "HTTP transport implementation for pull protocol.

   Internal namespace - use sg.flybot.pullable.remote for public API."
  (:require
   [clojure.string :as str]
   [clojure.walk]
   [sg.flybot.pullable.impl :as pattern]
   [sg.flybot.pullable.util :refer [contains-variables?]]
   [sg.flybot.pullable.collection :as coll]
   #?(:clj [cognitect.transit :as transit])
   #?(:clj [clojure.edn :as edn])
   #?(:clj [clojure.java.io :as io]))
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream])))

;;=============================================================================
;; Wire Serialization
;;=============================================================================

(defn- prepare-for-wire
  "Recursively convert Wireable objects to serializable data.
   Walks maps and vectors, converting any Wireable values."
  [x]
  (cond
    (satisfies? coll/Wireable x)
    (prepare-for-wire (coll/->wire x))

    (map? x)
    (persistent!
     (reduce-kv (fn [m k v]
                  (assoc! m k (prepare-for-wire v)))
                (transient {})
                x))

    (vector? x)
    (mapv prepare-for-wire x)

    (sequential? x)
    (map prepare-for-wire x)

    :else x))

;;=============================================================================
;; Security: Safe Pattern Evaluation
;;=============================================================================

(def ^:private max-pattern-depth
  "Maximum nesting depth for patterns. Prevents stack overflow DoS."
  100)

(def ^:private safe-predicates
  "Whitelist of safe predicate functions for (? :pred ...) patterns.
   Only type-checking predicates - no side effects, no I/O."
  {'string?   string?
   'number?   number?
   'integer?  integer?
   'float?    float?
   'keyword?  keyword?
   'symbol?   symbol?
   'ident?    ident?
   'map?      map?
   'vector?   vector?
   'list?     list?
   'set?      set?
   'seq?      seq?
   'coll?     coll?
   'nil?      nil?
   'some?     some?
   'true?     true?
   'false?    false?
   'boolean?  boolean?
   'empty?    empty?
   'pos?      pos?
   'neg?      neg?
   'zero?     zero?
   'even?     even?
   'odd?      odd?
   'pos-int?  pos-int?
   'neg-int?  neg-int?
   'nat-int?  nat-int?
   'uuid?     uuid?
   'inst?     inst?})

(defn- safe-resolve
  "Sandboxed symbol resolver. Only allows whitelisted predicates."
  [sym]
  (or (get safe-predicates sym)
      (throw (ex-info "Predicate not allowed in remote patterns"
                      {:symbol sym
                       :allowed (keys safe-predicates)}))))

(defn- safe-eval
  "Sandboxed evaluator. Blocks all fn/fn* forms to prevent code execution."
  [form]
  (throw (ex-info "Code evaluation not allowed in remote patterns"
                  {:form form})))

(defn- pattern-depth
  "Calculate the nesting depth of a pattern."
  [pattern]
  (cond
    (map? pattern)
    (inc (reduce max 0 (map pattern-depth (vals pattern))))

    (sequential? pattern)
    (inc (reduce max 0 (map pattern-depth pattern)))

    :else 0))

(defn- validate-pattern-depth!
  "Throws if pattern exceeds maximum depth."
  [pattern]
  (let [depth (pattern-depth pattern)]
    (when (> depth max-pattern-depth)
      (throw (ex-info "Pattern too deeply nested"
                      {:depth depth
                       :max-depth max-pattern-depth})))))

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
       ;; NOTE: ?x symbols are valid Clojure symbols and read natively.
       ;; No custom reader needed.
       (edn/read-string s))))

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

  ;; parse-mutation: flat CREATE
  (parse-mutation '{:posts {nil {:title "New"}}})
  ;=> {:path [:posts], :query nil, :value {:title "New"}}

  ;; parse-mutation: flat UPDATE
  (parse-mutation '{:posts {{:id 3} {:title "Updated"}}})
  ;=> {:path [:posts], :query {:id 3}, :value {:title "Updated"}}

  ;; parse-mutation: flat DELETE
  (parse-mutation '{:posts {{:id 3} nil}})
  ;=> {:path [:posts], :query {:id 3}, :value nil}

  ;; parse-mutation: nested CREATE (role-based)
  (parse-mutation '{:member {:posts {nil {:title "New"}}}})
  ;=> {:path [:member :posts], :query nil, :value {:title "New"}}

  ;; parse-mutation: nested UPDATE (role-based)
  (parse-mutation '{:admin {:posts {{:id 1} {:title "X"}}}})
  ;=> {:path [:admin :posts], :query {:id 1}, :value {:title "X"}}

  ;; parse-mutation: returns nil for read patterns
  (parse-mutation '{:posts ?all})
  ;=> nil

  (parse-mutation '{:member {:me ?user}})
  ;=> nil

  ;; parse-mutation: keyword query keys are reads, not mutations
  (parse-mutation '{:config {:debug (?d :default false)}})
  ;=> nil

  (parse-mutation '{:users {:name "Alice"}})
  ;=> nil

  (parse-mutation '{:role {:config {:debug true}}})
  ;=> nil

  ;; parse-mutation: nested map with variables is a read, not mutation
  (parse-mutation '{:posts {{:id 1} {:title ?t}}})
  ;=> nil

  ;; parse-mutation: vector with variables is a read
  (parse-mutation '{:posts {{:id 1} {:tags [?first ?rest*]}}})
  ;=> nil

  ;; parse-mutation: extended var in nested map is a read
  (parse-mutation '{:posts {{:id 1} {:title (?t :when string?)}}})
  ;=> nil

  ;; parse-mutation: role-based nested read with variables
  (parse-mutation '{:admin {:posts {{:id 1} {:title ?t}}}}))
  ;=> nil)

;;=============================================================================
;; Response Helpers
;;=============================================================================

(defn- normalize-value
  "Normalize a value for serialization.
   Types implementing Wireable are converted via ->wire.
   Standard Clojure data passes through unchanged."
  [x]
  (clojure.walk/postwalk
   (fn [v]
     (if (satisfies? coll/Wireable v)
       (coll/->wire v)
       v))
   x))

(defn- success
  "Build success response containing only the vars bindings.
   The response IS the vars map directly - no wrapper."
  [_data vars]
  (normalize-value vars))

(defn- error [code reason & [path]]
  (cond-> {:code code :reason reason}
    path (assoc :path path)))

(defn- failure [err]
  {:errors (if (sequential? err) err [err])})

(defn- success? [response]
  (not (contains? response :errors)))

(def ^:private protocol-error-codes
  "HTTP status codes for protocol-level errors (not app-specific)."
  {:invalid-request    400
   :decode-error       400
   :schema-violation   403
   :binding-conflict   422
   :match-failure      422
   :invalid-collection 404
   :not-found          404
   :method-not-allowed 405
   :execution-error    500})

(defn- response->http-status
  "Determine HTTP status code from response.
   Success returns 200, errors return code based on error type.
   Uses protocol-error-codes merged with app-provided error-codes."
  ([response] (response->http-status response nil))
  ([response error-codes]
   (if (success? response)
     200
     (let [code (get-in response [:errors 0 :code])
           all-codes (merge protocol-error-codes error-codes)]
       (get all-codes code 400)))))

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
  {:body (encode (prepare-for-wire response) format)
   :content-type (get content-types format)})

(defn parse-mutation
  "Detect if pattern is a mutation. Returns {:path :query :value} or nil.

   Flat mutation patterns:
   - {:posts {nil data}}         -> CREATE (path=[:posts])
   - {:posts {{:id 3} data}}     -> UPDATE (path=[:posts])
   - {:posts {{:id 3} nil}}      -> DELETE (path=[:posts])

   Nested mutation patterns (role-based):
   - {:role/member {:posts {nil data}}}     -> CREATE (path=[:role/member :posts])
   - {:role/admin {:posts {{:id 1} data}}}  -> UPDATE (path=[:role/admin :posts])

   Read patterns return nil (value is or contains ?-prefixed symbols)."
  [pattern]
  (when (and (map? pattern) (= 1 (count pattern)))
    (let [[k1 v1] (first pattern)]
      (when (keyword? k1)
        (cond
          ;; Nested: {:role/member {:posts {query value}}}
          (and (map? v1)
               (= 1 (count v1))
               (keyword? (ffirst v1))
               (map? (val (first v1)))
               (= 1 (count (val (first v1)))))
          (let [[k2 v2] (first v1)
                [query value] (first v2)]
            (when (and (not (contains-variables? value))
                       (or (nil? query) (map? query)))
              {:path [k1 k2] :query query :value value}))

          ;; Flat: {:posts {query value}}
          (and (map? v1) (= 1 (count v1)))
          (let [[query value] (first v1)]
            (when (and (not (contains-variables? value))
                       (or (nil? query) (map? query)))
              {:path [k1] :query query :value value}))

          :else nil)))))

(defn- keyword->symbol
  "Convert keyword to symbol, preserving namespace."
  [k]
  (if (namespace k)
    (symbol (namespace k) (name k))
    (symbol (name k))))

(defn- execute-mutation
  "Execute a mutation against the API collection.
   Path can be flat [:posts] or nested [:member :posts].

   errors: {:detect fn, :codes map} from api-fn response
   If detect returns truthy, treats result as error with :type and :message."
  [api-fn ring-request {:keys [path query value]}]
  (try
    (let [{:keys [data errors]} (api-fn ring-request)
          {:keys [detect]} errors
          detect-fn (when detect
                      (if (keyword? detect) #(get % detect) detect))
          coll (get-in data path)
          result-key (last path)]
      (if (and coll (satisfies? coll/Mutable coll))
        (let [result (coll/mutate! coll query value)]
          (if-let [{:keys [type message]} (when detect-fn (detect-fn result))]
            (failure (error type (or message (name type)) (vec path)))
            (success {result-key result} {(keyword->symbol result-key) result})))
        (failure (error :invalid-collection
                        (str "Collection " (pr-str path) " not found or unavailable")))))
    (catch #?(:clj Exception :cljs js/Error) e
      (failure (error :execution-error
                      #?(:clj (.getMessage e) :cljs (.-message e)))))))

(defn execute
  "Execute a pull pattern against an api-fn. Returns vars map or {:errors [...]}.

   Used by both the Ring handler and direct callers (e.g., browser sandbox).

   api-fn:  (fn [context] {:data ... :schema ... :errors ...})
   pattern: Clojure data structure (EDN)
   opts:    {:params  {...}  ; $-param substitution
             :resolve fn     ; symbol resolver (default: safe whitelist)
             :eval-fn fn     ; form evaluator (default: blocked)
             :context map}   ; passed to api-fn (default: {})"
  ([api-fn pattern] (execute api-fn pattern {}))
  ([api-fn pattern opts]
   (try
     (let [resolved (resolve-params pattern (:params opts))
           _ (validate-pattern-depth! resolved)
           ctx (or (:context opts) {})]
       (if-let [mutation (parse-mutation resolved)]
         (execute-mutation api-fn ctx mutation)
         (let [{:keys [data schema]} (api-fn ctx)
               compiled (pattern/compile-pattern
                         resolved
                         (cond-> (select-keys opts [:resolve :eval-fn])
                           (not (:resolve opts)) (assoc :resolve safe-resolve)
                           (not (:eval-fn opts)) (assoc :eval-fn safe-eval)
                           schema (assoc :schema schema)))
               result (compiled (pattern/vmr data))]
           (if (pattern/failure? result)
             (failure (match-failure->error result))
             (success (:val result) (:vars result))))))
     (catch #?(:clj Exception :cljs js/Error) e
       (failure (error :execution-error
                       #?(:clj (.getMessage e) :cljs (.-message e))))))))

(defn- execute-pull
  "Execute pull pattern against API data. Delegates to `execute`."
  [api-fn ring-request pull-request]
  (execute api-fn (:pattern pull-request)
           {:params  (:params pull-request)
            :context ring-request}))

(defn- handle-pull [api-fn ring-request]
  (let [req-fmt (or (parse-content-type (get-in ring-request [:headers "content-type"]))
                    :transit-json)
        res-fmt (negotiate-format (get-in ring-request [:headers "accept"]))
        body (read-body (:body ring-request))
        ;; Get error codes from api-fn (call it to get :errors config)
        error-codes (get-in (api-fn ring-request) [:errors :codes])
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
    (ring-response (response->http-status response error-codes) body content-type)))

(defn- handle-schema [api-fn ring-request]
  (let [res-fmt (negotiate-format (get-in ring-request [:headers "accept"]))
        {:keys [schema sample]} (api-fn ring-request)
        response (if schema
                   (normalize-value (cond-> {:schema schema}
                                      sample (assoc :sample sample)))
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

   api-fn: (ring-request) -> {:data lazy-map, :schema schema-map, :errors errors-config}

   The :errors key in api-fn response configures error handling:
   - :detect - keyword or (fn [result] error-map-or-nil) to detect mutation errors
   - :codes  - Map of error-type to HTTP status (merged with protocol defaults)

   Collections return errors as data: {:error {:type :forbidden :message \"...\"}}

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

^:rct/test
(comment
  ;; Security: safe-resolve allows only whitelisted predicates
  (safe-resolve 'string?) ;=>> fn?
  (try (safe-resolve 'slurp) (catch Exception e (:symbol (ex-data e)))) ;=> 'slurp

  ;; Security: safe-eval blocks all code evaluation
  (try (safe-eval '(fn [x] x)) (catch Exception e (:form (ex-data e)))) ;=> '(fn [x] x)

  ;; Security: pattern depth validation
  (pattern-depth '{:a {:b {:c 1}}}) ;=> 3

  ;; Protocol error codes
  (get protocol-error-codes :invalid-request) ;=> 400
  (get protocol-error-codes :not-found) ;=> 404
  (get protocol-error-codes :execution-error) ;=> 500

  ;; response->http-status with custom codes
  (response->http-status {:errors [{:code :forbidden}]} {:forbidden 403}) ;=> 403
  (response->http-status {:errors [{:code :custom}]} {:custom 418}) ;=> 418
  (response->http-status {:errors [{:code :unknown}]} nil) ;=> 400

  ;; --- execute ---

  (require '[sg.flybot.pullable.collection :as coll])

  (def test-api-fn
    (let [src (coll/atom-source {:initial [{:id 1 :name "Alice"} {:id 2 :name "Bob"}]})
          items (coll/collection src)]
      (fn [_ctx] {:data {:users items}})))

  (execute test-api-fn '{:users ?all})
  ;=>> {'all vector?}

  (get (execute test-api-fn '{:users {{:id 1} ?u}}) 'u)
  ;=>> {:id 1 :name "Alice"}

  (get (execute test-api-fn '{:users {nil {:id 99 :name "Carol"}}}) 'users)
  ;=>> {:id 3 :name "Carol"}

  (execute test-api-fn '{:users {{:id 2} nil}})
  ;=>> {'users true}

  (:errors (execute test-api-fn '{:users (?x :when string?)}))
  ;=>> [{:code :match-failure}]

  (:errors (execute test-api-fn '{:missing {nil {:id 1}}}))
  ;=>> [{:code :invalid-collection}]

  (get (execute test-api-fn '{:users {{:id $uid} ?u}} {:params {:uid 1}}) 'u)
  ;=>> {:id 1 :name "Alice"}
  )

