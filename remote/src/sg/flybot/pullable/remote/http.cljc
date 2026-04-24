(ns ^:no-doc sg.flybot.pullable.remote.http
  "HTTP transport implementation for pull protocol.

   Internal namespace - use sg.flybot.pullable.remote for public API."
  (:require
   [clojure.string :as str]
   [clojure.walk]
   [sg.flybot.pullable.impl :as pattern]
   [sg.flybot.pullable.util :refer [contains-variables? variable?]]
   [sg.flybot.pullable.collection :as coll]
   #?(:clj [cognitect.transit :as transit])
   #?(:clj [clojure.edn :as edn])
   #?(:clj [clojure.java.io :as io]))
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream])))

;;=============================================================================
;; Protocol Utilities
;;=============================================================================

(defn- direct-satisfies?
  "Fast alternative to `clojure.core/satisfies?`. True when `v`'s class directly
   implements the protocol interface (covers `deftype` inline and `reify`) OR is
   registered in the protocol's `:impls` map (covers `extend-type` /
   `extend-protocol`).

   Does not cover `:extend-via-metadata` — values whose protocol impl is attached
   via metadata aren't detected."
  [protocol v]
  #?(:clj (or (instance? (:on-interface protocol) v)
              (some? (get (:impls protocol) (class v))))
     :cljs (satisfies? protocol v)))

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
  (parse-mutation '{:admin {:posts {{:id 1} {:title ?t}}}})
  ;=> nil

  ;; parse-mutation: deeply nested CREATE
  (parse-mutation '{:a {:member {:posts {nil {:title "New"}}}}})
  ;=> {:path [:a :member :posts], :query nil, :value {:title "New"}}

  ;; parse-mutation: deeply nested UPDATE
  (parse-mutation '{:a {:member {:posts {{:id 1} {:title "X"}}}}})
  ;=> {:path [:a :member :posts], :query {:id 1}, :value {:title "X"}}

  ;; parse-mutation: deeply nested DELETE
  (parse-mutation '{:a {:member {:posts {{:id 1} nil}}}})
  ;=> {:path [:a :member :posts], :query {:id 1}, :value nil}

  ;; parse-mutation: deeply nested read returns nil
  (parse-mutation '{:a {:member {:posts ?all}}})
  ;=> nil

  ;; parse-mutation: deep config (not mutation) returns nil
  (parse-mutation '{:a {:b {:c {:debug true}}}}))
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
     (if (direct-satisfies? coll/Wireable v)
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
  ;; Wireable conversion already happened in `success` via `normalize-value`
  ;; (and in `handle-schema` for schema responses). No need to re-walk here.
  {:body (encode response format)
   :content-type (get content-types format)})

(defn parse-mutation
  "Detect if pattern is a mutation. Returns {:path :query :value} or nil.

   Recursively walks single-key keyword maps until it finds a mutation leaf:
   - {nil data}         -> CREATE
   - {{:id 3} data}     -> UPDATE
   - {{:id 3} nil}      -> DELETE

   Supports arbitrary nesting depth:
   - {:posts {nil data}}                          -> path [:posts]
   - {:role/member {:posts {nil data}}}           -> path [:role/member :posts]
   - {:a {:role/member {:posts {nil data}}}}      -> path [:a :role/member :posts]

   Read patterns return nil (value is or contains ?-prefixed symbols)."
  [pattern]
  (when (and (map? pattern) (= 1 (count pattern)))
    (let [[k v] (first pattern)]
      (when (keyword? k)
        (if (and (map? v) (= 1 (count v))
                 (let [inner-k (ffirst v)]
                   (or (nil? inner-k) (map? inner-k))))
          ;; Terminal: {query value} — the mutation leaf
          (let [[query value] (first v)]
            (when (and (not (contains-variables? value))
                       (or (nil? query) (map? query)))
              {:path [k] :query query :value value}))
          ;; Recursive: {:nested-key deeper-pattern}
          (when-let [inner (parse-mutation v)]
            (update inner :path #(into [k] %))))))))

(defn- keyword->symbol
  "Convert keyword to symbol, preserving namespace."
  [k]
  (if (namespace k)
    (symbol (namespace k) (name k))
    (symbol (name k))))

(defn- make-detect-fn
  "Build detect function from :detect config.
   Keyword → (fn [v] (get v kw)), function → wrapped with error context, nil → nil.
   User-provided functions are wrapped so exceptions surface with a descriptive message.

   Contract: detect-fn must return nil (no error) or a map with :type and
   optional :message. Non-map truthy returns (e.g. strings, keywords) will
   cause downstream errors in path-walking and error classification."
  [detect]
  (when detect
    (if (keyword? detect)
      #(get % detect)
      (fn [v]
        (try
          (detect v)
          (catch #?(:clj Exception :cljs js/Error) e
            (throw (ex-info (str "Error detection function failed: "
                                 #?(:clj (.getMessage e) :cljs (.-message e)))
                            {:detect detect}
                            e))))))))

(defn- path-prefix?
  "True if `prefix` is a prefix of `path` (inclusive).
   An empty prefix [] matches any path.
   Equal paths match (e.g. [:a] is a prefix of [:a])."
  [prefix path]
  (let [pc (count prefix)
        pathc (count path)]
    (and (<= pc pathc)
         (or (zero? pc)
             (= prefix (subvec (vec path) 0 pc))))))

(defn- var-binding-sym
  "For a pattern variable symbol, return the matcher's binding symbol
   (same rule `pattern/matching-var-rewrite` uses), or nil for wildcards
   (`?_`, `?_?`, `?_*`, ...) and the bare `?` core-form marker.
   Delegates quantifier parsing to `pattern/parse-matching-var`."
  [s]
  (if-let [{:keys [sym]} (pattern/parse-matching-var s)]
    ;; Quantified: :sym is `?x` with ? prefix, or nil for ?_* etc.
    (when sym (symbol (subs (name sym) 1)))
    ;; Plain ?x or ?_ (parse-matching-var returns nil for unquantified)
    (let [nm (name s)]
      (when (and (> (count nm) 1) (not= "?_" nm))
        (symbol (subs nm 1))))))

(defn- pattern-var-bindings
  "Walk pattern, return {sym path} for each bound variable.
   Descends into maps, vectors, and extended `(?x :when ...)` forms.
   Vector/sequence elements share the enclosing map path so that error
   detection covers the whole sub-pattern; individual element positions
   are not distinguished. Wildcards (`?_`-family) are skipped."
  [pattern]
  (letfn [(bound-sym [x]
            (cond
              (variable? x) x
              (and (seq? x) (variable? (first x))) (first x)))
          (walk [acc p prefix]
            (cond
              (map? p)
              (reduce-kv (fn [a k v] (walk a v (conj prefix k))) acc p)

              (vector? p)
              (reduce (fn [a v] (walk a v prefix)) acc p)

              :else
              (if-let [bound (some-> (bound-sym p) var-binding-sym)]
                (assoc acc bound prefix)
                acc)))]
    (walk {} pattern [])))

^:rct/test
(comment
  (pattern-var-bindings '{:a ?x :b ?y})
  ;=>> {'x [:a] 'y [:b]}

  (pattern-var-bindings '{:a {:b ?x}})
  ;=> {'x [:a :b]}

  (pattern-var-bindings '{:a (?x :when string?)})
  ;=> {'x [:a]}

  (pattern-var-bindings '{:a "literal" :b ?x})
  ;=> {'x [:b]}

  (pattern-var-bindings nil) ;=> {}

  ;; quantifier suffixes are stripped to match matcher binding names
  (pattern-var-bindings '{:a ?x?})  ;=> {'x [:a]}
  (pattern-var-bindings '{:a ?x*})  ;=> {'x [:a]}
  (pattern-var-bindings '{:a ?x+!}) ;=> {'x [:a]}

  ;; vector elements share the enclosing map path
  (pattern-var-bindings '{:items [?first ?rest*]})
  ;=>> {'first [:items] 'rest [:items]}

  ;; nested vectors
  (pattern-var-bindings '{:a [[?x ?y]]})
  ;=>> {'x [:a] 'y [:a]}

  ;; path-prefix?: empty prefix matches any path
  (path-prefix? [] [:a]) ;=> true
  (path-prefix? [] [:a :b]) ;=> true

  ;; path-prefix?: equal paths match
  (path-prefix? [:a] [:a]) ;=> true

  ;; path-prefix?: longer prefix doesn't match shorter path
  (path-prefix? [:a :b] [:a]) ;=> false
  )

(defn- error-map->errors
  "Convert error-map {path error-data} into wire-format error vectors.
   Error paths are always pattern-derived, so no filtering is needed —
   all errors are relevant by construction."
  [error-map]
  (when (seq error-map)
    (mapv (fn [[path err]]
            (cond-> {:code   (or (:type err) :unknown)
                     :reason (or (:message err) "Unknown error")}
              (seq path) (assoc :path path)))
          error-map)))

(defn- detect-path-error
  "Walk path checking for errors at each step via detect-fn.
   Only checks standard maps (via `map?` guard) — pure ILookup implementations
   pass through unchecked because `map?` returns false for reified ILookup,
   and calling detect-fn on them could trigger side effects or lazy evaluation.
   Returns [value nil] on success, [nil err-map] with :path on error."
  [data path detect-fn]
  (loop [m data, [k & ks] path, traversed []]
    (if-not k
      [m nil]
      (if-let [err (when (and detect-fn (map? m)) (detect-fn m))]
        [nil (assoc err :path traversed)]
        (recur (get m k) ks (conj traversed k))))))

^:rct/test
(comment
  (def err-detect #(get % :error))

  (detect-path-error {:a {:b 42}} [:a :b] err-detect)
  ;=> [42 nil]

  (detect-path-error {:a {:error {:type :forbidden}} :b 1} [:a :b] err-detect)
  ;=> [nil {:type :forbidden :path [:a]}]

  ;; Walks through ILookup (mutation path finding)
  (let [stub (reify clojure.lang.ILookup
               (valAt [_ k] (case k :resource {:error {:type :forbidden}} nil))
               (valAt [this k _] (.valAt this k)))]
    (detect-path-error {:role stub} [:role :resource :name] err-detect))
  ;=> [nil {:type :forbidden :path [:role :resource]}]

  (detect-path-error {:x 1} [] err-detect) ;=> [{:x 1} nil]
  (detect-path-error {:a {:b 42}} [:a :b] nil) ;=> [42 nil]
  )

(defn- trim-pattern
  "Remove pattern keys at or under paths in `error-paths`. Descends into
   sub-patterns only when the current path is a prefix of some error path.
   Returns trimmed pattern, or nil if all keys were removed."
  ([pattern error-paths]
   (trim-pattern pattern error-paths []))
  ([pattern error-paths current-path]
   (when (and (map? pattern)
              (not (contains? error-paths current-path)))
     (let [trimmed (reduce-kv
                    (fn [acc k v]
                      (let [child-path (conj current-path k)]
                        (cond
                          (contains? error-paths child-path)
                          acc
                          (and (map? v)
                               (some #(path-prefix? child-path %) error-paths))
                          (let [v' (trim-pattern v error-paths child-path)]
                            (if (seq v') (assoc acc k v') acc))
                          :else
                          (assoc acc k v))))
                    {}
                    pattern)]
       (when (seq trimmed) trimmed)))))

(defn- detect-read-errors
  "Pre-match error detection: walk `data` along each var path through
   plain maps only. Stops at any non-map value — ILookup implementations
   are not probed, matching the contract that `{:error ...}` must live in
   plain data (errors returned from ILookup.valAt are invisible here).

   Checks detect-fn at every map encountered, including the leaf, so both
   intermediate role-gate errors and leaf errors surface. Returns
   {err-path err-data} or nil."
  [data var-paths detect-fn]
  (when (and detect-fn (seq var-paths))
    (let [errors
          (reduce
           (fn [acc path]
             (loop [m data, [k & ks] path, traversed []]
               (let [err (when (map? m) (detect-fn m))]
                 (cond
                   (not (map? m)) acc
                   err            (cond-> acc
                                    (not (contains? acc traversed))
                                    (assoc traversed err))
                   (nil? k)       acc
                   :else          (recur (get m k) ks (conj traversed k))))))
           {}
           var-paths)]
      (when (seq errors) errors))))

^:rct/test
(comment
  ;; trim-pattern — removes known error paths
  (trim-pattern '{:a {:x ?x} :b {:y ?y}} #{[:b]})
  ;=> '{:a {:x ?x}}

  ;; trim-pattern — nested removal
  (trim-pattern '{:section {:ok {:name ?n} :denied {:name ?d}}}
                #{[:section :denied]})
  ;=> '{:section {:ok {:name ?n}}}

  ;; trim-pattern — all keys errored → nil
  (trim-pattern '{:a {:x ?x} :b {:y ?y}} #{[:a] [:b]}) ;=> nil

  ;; trim-pattern — sub-pattern entirely empty after trim → parent removed
  (trim-pattern '{:section {:denied {:name ?d}}} #{[:section :denied]}) ;=> nil

  ;; trim-pattern — root error path trims entire pattern
  (trim-pattern '{:a ?x :b ?y} #{[]}) ;=> nil

  ;; detect-read-errors — intermediate error at role gate
  (detect-read-errors {:member {:error {:type :forbidden :message "No"}}}
                      #{[:member :posts :x]}
                      err-detect)
  ;=> {[:member] {:type :forbidden :message "No"}}

  ;; detect-read-errors — leaf error (var binds directly to error map)
  (detect-read-errors {:a {:error {:type :forbidden}}}
                      #{[:a]}
                      err-detect)
  ;=> {[:a] {:type :forbidden}}

  ;; detect-read-errors — root-level error
  (detect-read-errors {:error {:type :forbidden :message "Unauth"}}
                      #{[:a] [:b]}
                      err-detect)
  ;=> {[] {:type :forbidden :message "Unauth"}}

  ;; detect-read-errors — stops at ILookup (does not probe)
  (let [stub (reify clojure.lang.ILookup
               (valAt [_ k] (case k :admin {:error {:type :forbidden}} nil))
               (valAt [this k _] (.valAt this k)))]
    (detect-read-errors {:section stub} #{[:section :admin :x]} err-detect))
  ;=> nil

  ;; detect-read-errors — no errors
  (detect-read-errors {:a {:b 42}} #{[:a :b]} err-detect) ;=> nil

  ;; detect-read-errors — nil detect-fn
  (detect-read-errors {:a {:error {}}} #{[:a]} nil) ;=> nil
  )

(defn- detect-val-errors
  "Walk each bound var's path in `result`'s `:val`; for paths `detect-fn`
   flags, record the error and drop the corresponding var from `:vars`.
   Returns {:kept-vars <matcher vars minus errored symbols>
            :val-errs  <wire-format errors, deduped by path>}.
   `var-bindings` drives the error walk only — `kept-vars` is seeded from
   the matcher's full `vars` so variables in vector/sequence patterns (which
   share a single enclosing path) still surface in the response.
   When `detect-fn` is nil or `var-bindings` empty, returns {:kept-vars vars}."
  [{:keys [val vars]} var-bindings detect-fn]
  (if (or (nil? detect-fn) (empty? var-bindings))
    {:kept-vars vars}
    (let [{:keys [err-map errored]}
          (reduce-kv
           (fn [acc sym path]
             (let [[v e] (detect-path-error val path detect-fn)
                   err   (or e (when (map? v) (detect-fn v)))]
               (if err
                 (let [ep (or (:path err) path)]
                   (-> acc
                       (update :errored conj sym)
                       (cond-> (not (contains? (:err-map acc) ep))
                         (assoc-in [:err-map ep] (dissoc err :path)))))
                 acc)))
           {:err-map {} :errored #{}}
           var-bindings)]
      {:kept-vars (reduce dissoc vars errored)
       :val-errs  (error-map->errors err-map)})))

^:rct/test
(comment
  (def err-detect #(get % :error))

  ;; partial: one path errored, one kept
  (detect-val-errors {:val  {:pub  {:items [1 2 3]}
                             :priv {:error {:type :forbidden :message "NA"}}}
                      :vars '{all [1 2 3] secret nil}}
                     '{all [:pub :items] secret [:priv :items]}
                     err-detect)
  ;=> {:kept-vars '{all [1 2 3]}
  ;    :val-errs  [{:code :forbidden :reason "NA" :path [:priv]}]}

  ;; all covered
  (detect-val-errors {:val  {:a {:error {:type :forbidden}} :b {:error {:type :forbidden}}}
                      :vars '{x nil y nil}}
                     '{x [:a :foo] y [:b :bar]}
                     err-detect)
  ;=>> {:kept-vars empty?}

  ;; leaf error: var binds directly to error map
  (:kept-vars
   (detect-val-errors {:val  {:a {:error {:type :forbidden :message "NA"}}}
                       :vars '{x {:error {:type :forbidden :message "NA"}}}}
                      '{x [:a]}
                      err-detect))
  ;=> {}

  ;; dedup: multiple vars under the same error path produce one error
  (:val-errs
   (detect-val-errors {:val  {:p {:error {:type :forbidden :message "denied"}}}
                       :vars '{x nil y nil}}
                      '{x [:p :a] y [:p :b]}
                      err-detect))
  ;=> [{:code :forbidden :reason "denied" :path [:p]}]

  ;; no detect-fn: vars pass through
  (detect-val-errors {:val {:a 1} :vars '{x 1}} '{x [:a]} nil)
  ;=> {:kept-vars '{x 1}}

  ;; empty var-bindings: vars pass through
  (detect-val-errors {:val {} :vars nil} {} err-detect)
  ;=> {:kept-vars nil}

  ;; vector-pattern vars survive even though var-bindings collapses them
  ;; onto a single enclosing path
  (detect-val-errors {:val  {:items [10 20 30]}
                      :vars '{first 10 rest (20 30)}}
                     '{first [:items] rest [:items]}
                     err-detect)
  ;=>> {:kept-vars {'first 10 'rest '(20 30)}}

  ;; vector-pattern error: both vars drop and error surfaces
  (detect-val-errors {:val  {:items {:error {:type :forbidden :message "NA"}}}
                      :vars '{first [:error {:type :forbidden :message "NA"}] rest ()}}
                     '{first [:items] rest [:items]}
                     err-detect)
  ;=> {:kept-vars {}
  ;    :val-errs  [{:code :forbidden :reason "NA" :path [:items]}]}
  )

(defn- execute-mutation
  "Execute a mutation against the API collection.
   Path can be flat [:posts] or nested [:member :posts].

   errors: {:detect fn, :codes map} from api-fn response
   If detect returns truthy, treats result as error with :type and :message."
  [api-fn ring-request {:keys [path query value]}]
  (try
    (let [{:keys [data errors]} (api-fn ring-request)
          detect-fn (make-detect-fn (:detect errors))
          [coll path-err] (detect-path-error data path detect-fn)
          result-key (last path)
          res (if path-err
                (let [{:keys [type message]} path-err]
                  (failure (error type (or message (name type)) (:path path-err))))
                (if (and coll (direct-satisfies? coll/Mutable coll))
                  (let [result (coll/mutate! coll query value)]
                    (if-let [{:keys [type message]} (when detect-fn (detect-fn result))]
                      (failure (error type (or message (name type)) (vec path)))
                      (success {result-key result} {(keyword->symbol result-key) result})))
                  (failure (error :invalid-collection
                                  (str "Collection " (pr-str path) " not found or unavailable")))))]
      (vary-meta res assoc ::error-codes (:codes errors)))
    (catch #?(:clj Exception :cljs js/Error) e
      (failure (error :execution-error
                      #?(:clj (.getMessage e) :cljs (.-message e)))))))

(defn- execute-read
  "Execute a read pattern in three phases:

   1. Walk raw `data` along each var path through plain maps only, collect
      errors (via `detect-read-errors`), and trim the pattern at those
      paths. Catches role-gate denials like `{:member {:error ...}}`
      before the matcher descends past them and fails with
      `:match-failure`.

   2. Match the trimmed pattern. Untrimmed branches see the original data;
      ILookup collections are invoked at most once.

   3. Walk the matcher's realized `:val` over the *trimmed* pattern's
      var-bindings (via `detect-val-errors`) for errors inside
      materialized data — leaf error maps or errors inside ILookup
      returns the matcher descended into. Scoping the walk to trimmed
      bindings avoids re-reporting step-1 errors that map passthrough
      otherwise leaks back into `:val`.

   Returns partial success when some var-paths resolve cleanly, or full
   failure when every var-path is error-covered.

   Note: `{:error ...}` returned from `ILookup.valAt` is invisible — the
   data walk stops at ILookup, and on match failure `:val` is nil.
   Errors must live in plain data."
  [api-fn ctx pattern opts]
  (let [{:keys [data schema errors]} (api-fn ctx)
        detect-fn    (make-detect-fn (:detect errors))
        var-bindings (when detect-fn (pattern-var-bindings pattern))
        data-err-map (detect-read-errors data (set (vals var-bindings)) detect-fn)
        trimmed      (if (seq data-err-map)
                       (trim-pattern pattern (set (keys data-err-map)))
                       pattern)
        data-errs    (vec (error-map->errors data-err-map))]
    (-> (if-not trimmed
          (failure data-errs)
          (let [compiled (pattern/compile-pattern
                          trimmed
                          (cond-> (select-keys opts [:resolve :eval-fn])
                            (not (:resolve opts)) (assoc :resolve safe-resolve)
                            (not (:eval-fn opts)) (assoc :eval-fn safe-eval)
                            schema (assoc :schema schema)))
                result   (compiled (pattern/vmr data))
                {:keys [kept-vars val-errs]}
                (detect-val-errors result (pattern-var-bindings trimmed) detect-fn)
                all-errs (into data-errs val-errs)]
            (cond
              (pattern/failure? result)
              (let [perr (match-failure->error result)]
                (failure
                 (if (some #(path-prefix? (:path %) (:path perr)) all-errs)
                   all-errs
                   (conj all-errs perr))))
              (and (seq all-errs) (empty? kept-vars))
              (failure all-errs)
              :else
              (cond-> (success (:val result) kept-vars)
                (seq all-errs) (vary-meta assoc ::detected-errors all-errs)))))
        (vary-meta assoc ::error-codes (:codes errors)))))

(defn execute
  "Execute a pull pattern against an api-fn. Returns vars map or {:errors [...]}.
   On partial success, detected errors are attached as ::detected-errors metadata.

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
           _        (validate-pattern-depth! resolved)
           ctx      (or (:context opts) {})]
       (if-let [mutation (parse-mutation resolved)]
         (execute-mutation api-fn ctx mutation)
         (execute-read api-fn ctx resolved opts)))
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
        error-codes (::error-codes (meta response))
        detected-errors (::detected-errors (meta response))
        wire-response (cond-> response
                        (seq detected-errors) (assoc :errors detected-errors))
        {:keys [body content-type]} (encode-response wire-response res-fmt)]
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
  ;;
  ;; Single api-fn exercises: reads, mutations, error detection, schema.
  ;; - :public  → read-only collection
  ;; - :guarded → wrap-mutable with ownership check (update returns :error)
  ;; - :private → role-gated error branch (like with-role returning {:error ...})
  ;; - :restricted → has schema (only :name key declared)

  (require '[sg.flybot.pullable.collection :as coll])

  (def test-api-fn
    (let [src (coll/atom-source {:initial [{:id 1 :name "Alice"} {:id 2 :name "Bob"}]})
          items (coll/collection src)
          guarded (coll/wrap-mutable items
                                     (fn [coll query value]
                                       (if (some? query)
                                         {:error {:type :forbidden :message "Not yours"}}
                                         (coll/mutate! coll query value))))]
      (fn [_ctx]
        {:data   {:public     {:items (coll/read-only items)}
                  :guarded    {:items guarded}
                  :private    {:error {:type :forbidden :message "Not authorized"}}
                  :restricted {:name "Alice" :secret "s3cret"}}
         :schema {:public :any :guarded :any :private :any
                  :restricted {:name :string}}
         :errors {:detect :error
                  :codes  {:forbidden 403}}})))

  ;; --- reads ---

  (execute test-api-fn '{:public {:items ?all}})
  ;=>> {'all vector?}

  (get (execute test-api-fn '{:public {:items {{:id 1} ?u}}}) 'u)
  ;=>> {:id 1 :name "Alice"}

  (:errors (execute test-api-fn '{:public {:items (?x :when string?)}}))
  ;=>> [{:code :match-failure}]

  (get (execute test-api-fn '{:public {:items {{:id $uid} ?u}}} {:params {:uid 1}}) 'u)
  ;=>> {:id 1 :name "Alice"}

  ;; --- read: schema violation ---

  (:errors (execute test-api-fn '{:restricted {:secret ?s}}))
  ;=>> [{:code :schema-violation}]

  ;; declared key works
  (execute test-api-fn '{:restricted {:name ?n}})
  ;=>> {'n "Alice"}

  ;; --- read: error detection ---

  ;; single error path → :forbidden
  (:errors (execute test-api-fn '{:private {:items ?all}}))
  ;=>> [{:code :forbidden :reason "Not authorized" :path [:private]}]

  ;; partial success: good branch succeeds, error branch in metadata
  (let [r (execute test-api-fn '{:public {:items ?all} :private {:items ?secret}})]
    [(get r 'all) (::detected-errors (meta r))])
  ;=>> [vector? [{:code :forbidden :reason "Not authorized" :path [:private]}]]

  ;; all error paths → all errors in response
  (def test-multi-error-api-fn
    (fn [_ctx]
      {:data   {:a {:error {:type :forbidden :message "Role :a required"}}
                :b {:error {:type :forbidden :message "Role :b required"}}}
       :errors {:detect :error :codes {:forbidden 403}}}))

  (let [errs (:errors (execute test-multi-error-api-fn '{:a {:x ?x} :b {:y ?y}}))]
    [(count errs)
     (set (map :code errs))
     (set (map :path errs))])
  ;=>> [2 #{:forbidden} #{[:a] [:b]}]

  ;; --- read: root-level error detection ---

  (def test-root-error-api-fn
    (fn [_ctx]
      {:data   {:error {:type :forbidden :message "Not authenticated"}}
       :errors {:detect :error :codes {:forbidden 403}}}))

  ;; root-level error → all paths fail, no :path in error (root)
  (:errors (execute test-root-error-api-fn '{:items ?all}))
  ;=>> [{:code :forbidden :reason "Not authenticated"}]

  ;; root-level error → multi-key pattern still fails entirely
  (:errors (execute test-root-error-api-fn '{:a ?x :b ?y}))
  ;=>> [{:code :forbidden :reason "Not authenticated"}]

  ;; --- read: nested error detection ---

  (def test-nested-error-api-fn
    (fn [_ctx]
      {:data   {:section {:ok {:name "Alice"}
                          :denied {:error {:type :forbidden :message "Access denied"}}}}
       :errors {:detect :error
                :codes  {:forbidden 403}}}))

  ;; nested error path → :forbidden with nested path
  (:errors (execute test-nested-error-api-fn '{:section {:denied {:name ?n}}}))
  ;=>> [{:code :forbidden :reason "Access denied" :path [:section :denied]}]

  ;; sibling of nested error → succeeds, no leaked errors
  (let [r (execute test-nested-error-api-fn '{:section {:ok {:name ?n}}})]
    [(get r 'n) (::detected-errors (meta r))])
  ;=>> ["Alice" nil]

  ;; mixed: ok succeeds, denied error in metadata → partial success
  (let [r (execute test-nested-error-api-fn '{:section {:ok {:name ?n} :denied {:name ?d}}})]
    [(get r 'n) (::detected-errors (meta r))])
  ;=>> ["Alice" [{:code :forbidden :reason "Access denied" :path [:section :denied]}]]

  ;; --- read: custom detect-fn ---

  (def test-custom-detect-api-fn
    (fn [_ctx]
      {:data   {:resource {:status :denied :reason "Access denied"}}
       :errors {:detect (fn [v] (when (and (map? v) (= :denied (:status v)))
                                  {:type :forbidden :message (:reason v)}))
                :codes  {:forbidden 403}}}))

  (:errors (execute test-custom-detect-api-fn '{:resource {:name ?n}}))
  ;=>> [{:code :forbidden :reason "Access denied" :path [:resource]}]

  ;; --- read: throwing detect-fn wraps with descriptive message ---

  (def test-throwing-detect-api-fn
    (fn [_ctx]
      {:data   {:items {:name "Alice"}}
       :errors {:detect (fn [_] (throw (ex-info "kaboom" {})))}}))

  (-> (execute test-throwing-detect-api-fn '{:items {:name ?n}})
      :errors first :reason)
  ;=>> #"Error detection function failed: kaboom"

  ;; --- read: without :errors config, error maps pass through as regular data ---

  (def test-no-detect-api-fn
    (fn [_ctx] {:data {:broken {:error {:type :forbidden}}}}))

  (execute test-no-detect-api-fn '{:broken ?x})
  ;=>> {'x {:error {:type :forbidden}}}

  ;; without :errors config, pattern failure returns generic :match-failure
  (def test-no-detect-fail-api-fn
    (fn [_ctx] {:data {:broken nil}}))

  (:errors (execute test-no-detect-fail-api-fn '{:broken {:deep ?v}}))
  ;=>> [{:code :match-failure}]

  ;; --- read: vector patterns preserve their bindings in the response ---

  (def test-vec-api-fn
    (fn [_ctx]
      {:data {:items [10 20 30] :meta {:count 3}}
       :errors {:detect :error :codes {:forbidden 403}}}))

  (execute test-vec-api-fn '{:items [?first ?rest*] :meta {:count ?c}})
  ;=>> {'first 10 'rest '(20 30) 'c 3}

  ;; vector-leaf error path is detected; remaining vars survive
  (def test-vec-error-api-fn
    (fn [_ctx]
      {:data {:ok {:n 1}
              :denied {:error {:type :forbidden :message "NA"}}}
       :errors {:detect :error :codes {:forbidden 403}}}))

  (let [r (execute test-vec-error-api-fn '{:ok {:n ?n} :denied [?items*]})]
    [(get r 'n) (contains? r 'items) (::detected-errors (meta r))])
  ;=>> [1 false [{:code :forbidden :reason "NA" :path [:denied]}]]

  ;; --- read: quantifier suffixes (?x?, ?x*, ?x+) bind to stripped name ---

  (execute (fn [_ctx] {:data {:items 42} :errors {:detect :error}})
           '{:items ?x?})
  ;=>> {'x 42}

  ;; --- mutations ---

  ;; create succeeds through guarded collection
  (get (execute test-api-fn '{:guarded {:items {nil {:name "Carol"}}}}) 'items)
  ;=>> {:id 3 :name "Carol"}

  ;; update on guarded → :forbidden (ownership violation detected by :detect)
  (:errors (execute test-api-fn '{:guarded {:items {{:id 1} {:name "Bob"}}}}))
  ;=>> [{:code :forbidden :reason "Not yours" :path [:guarded :items]}]

  ;; mutation to missing collection → :invalid-collection
  (:errors (execute test-api-fn '{:missing {nil {:id 1}}}))
  ;=>> [{:code :invalid-collection}]

  ;; mutation through error map (role-gated) → :forbidden (detected before collection)
  (:errors (execute test-api-fn '{:private {:items {nil {:name "X"}}}}))
  ;=>> [{:code :forbidden :reason "Not authorized" :path [:private]}]

  ;; --- exceptions ---

  ;; mutate! throws → :execution-error
  (def test-throwing-api-fn
    (let [throwing-coll (reify
                          clojure.lang.ILookup
                          (valAt [_ _] nil)
                          (valAt [_ _ nf] nf)
                          coll/Mutable
                          (mutate! [_ _ _]
                            (throw (ex-info "DB connection lost" {}))))]
      (fn [_ctx] {:data {:items throwing-coll}})))

  (:errors (execute test-throwing-api-fn '{:items {nil {:name "X"}}}))
  ;=>> [{:code :execution-error :reason "DB connection lost"}]

  ;; api-fn throws on read → :execution-error
  (def test-crashing-api-fn
    (fn [_ctx] (throw (ex-info "Service unavailable" {}))))

  (:errors (execute test-crashing-api-fn '{:x ?x}))
  ;=>> [{:code :execution-error :reason "Service unavailable"}]

  ;; api-fn throws on mutation → :execution-error
  (:errors (execute test-crashing-api-fn '{:x {nil {:a 1}}}))
  ;=>> [{:code :execution-error :reason "Service unavailable"}]

  ;; collection throws on read → :execution-error (exception propagates)
  (def throwing-read-coll
    (reify
      clojure.lang.ILookup
      (valAt [_ _] (throw (ex-info "DB connection lost" {})))
      (valAt [_ _ _] (throw (ex-info "DB connection lost" {})))))

  (:errors (execute (fn [_ctx] {:data {:broken {:items throwing-read-coll}}})
                    '{:broken {:items {{:id 1} ?x}}}))
  ;=>> [{:code :execution-error :reason "DB connection lost"}]
  )

