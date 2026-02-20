(ns sg.flybot.pullable.schema
  "Schema validation and filtering for pullable patterns.

   This namespace provides:
   - Schema registry for type inference rules
   - Pattern-schema validation (pre-compilation)
   - Value filtering based on schema structure

   Schema rules are functions: (schema) -> {:type :child-schema :valid-keys} | nil
   Return nil to indicate the rule doesn't apply to this schema.")

;;=============================================================================
;; Schema Registry
;;=============================================================================

(defonce ^:private schema-rules (atom []))

(defn register-schema-rule!
  "Register a rule for schema type inference.
   Rule: (schema) -> {:type t, :child-schema fn, :valid-keys set} | nil
   Return nil if rule doesn't apply. Rules tried in reverse order."
  [rule]
  (swap! schema-rules conj rule))

(defn get-schema-info
  "Get type info from a schema using registered rules.
   Returns {:type keyword, :child-schema fn-or-nil, :valid-keys set-or-nil}."
  [schema]
  (or (some #(% schema) (reverse @schema-rules))
      {:type :any}))

(defn ^:no-doc infer-schema-type
  "Infer the type keyword from a schema value."
  [schema]
  (:type (get-schema-info schema)))

;;=============================================================================
;; Schema Validation
;;=============================================================================

(def ^:private subseq-pattern-types
  "Internal pattern types for sequence matching - always valid"
  #{:one :repeat :optional :term :subseq})

(defn- indexed-lookup-key?
  "Check if key is an indexed lookup (non-keyword map key for ILookup access)."
  [k]
  (not (keyword? k)))

(defn ^:no-doc validate-pattern-schema!
  "Validate pattern against schema. Throws on violation.
   core-pattern? checks if x is a (? :type ...) pattern."
  [ptn schema core-pattern?]
  (when (and schema (core-pattern? ptn))
    (let [ptn-type (second ptn)
          {:keys [type child-schema valid-keys indexed-lookup?]} (get-schema-info schema)
          type-ok? (or (subseq-pattern-types ptn-type)
                       (case type
                         :any true
                         :map (#{:map :var :any} ptn-type)
                         ;; :map allowed on :seq only if schema has :ilookup annotation
                         :seq (or (#{:seq :var :any} ptn-type)
                                  (and (= ptn-type :map) indexed-lookup?))
                         (:number :string :keyword :symbol :boolean)
                         (#{:var :val :pred :any :regex :-> :sub} ptn-type)
                         true))]
      (when-not type-ok?
        (throw (ex-info (str "Schema violation: pattern :" ptn-type " vs schema :" type
                             (when (and (= ptn-type :map) (= type :seq))
                               " (add :ilookup to schema for indexed lookup)"))
                        {:pattern-type ptn-type :schema-type type :schema schema})))
      (case ptn-type
        :map
        (doseq [[k child] (partition 2 (drop 2 ptn))]
          (if (indexed-lookup-key? k)
            ;; Indexed lookup: schema must be :seq with :ilookup, validate child against element schema
            (when child-schema
              (validate-pattern-schema! child (child-schema 0) core-pattern?))
            ;; Keyword key: must be on :map schema
            (do
              (when (and valid-keys (not (valid-keys k)))
                (throw (ex-info (str "Key " k " not in schema")
                                {:key k :valid-keys valid-keys})))
              (when child-schema
                (validate-pattern-schema! child (child-schema k) core-pattern?)))))
        :seq
        (when child-schema
          (doseq [[i child] (map-indexed vector (drop 2 ptn))]
            (validate-pattern-schema! child (child-schema i) core-pattern?)))
        ;; Other pattern types
        (doseq [child (drop 2 ptn)]
          (when (core-pattern? child)
            (validate-pattern-schema! child schema core-pattern?)))))))

;;=============================================================================
;; Schema Filtering
;;=============================================================================

(defn ^:no-doc filter-by-schema
  "Filter value to only include keys defined in schema."
  [value schema]
  (let [{:keys [valid-keys type child-schema]} (get-schema-info schema)]
    (cond
      (and valid-keys (= type :map) (map? value))
      (reduce-kv (fn [acc k v]
                   (if (valid-keys k)
                     (assoc acc k (if child-schema (filter-by-schema v (child-schema k)) v))
                     acc))
                 {} value)

      (and (= type :seq) child-schema (sequential? value))
      (mapv #(filter-by-schema % (child-schema 0)) value)

      :else value)))

(defn ^:no-doc wrap-with-schema-filter
  "Wrap matcher to filter :val by schema. failure? checks for MatchFailure."
  [matcher schema failure?]
  (fn [mr]
    (let [result (matcher mr)]
      (if (failure? result) result (update result :val filter-by-schema schema)))))

;;=============================================================================
;; Built-in Schema Rules
;;=============================================================================

(defn ^:no-doc infer-value-type
  "Infer schema type from a runtime value."
  [v]
  (cond (map? v) :map, (sequential? v) :seq, (string? v) :string
        (keyword? v) :keyword, (symbol? v) :symbol, (number? v) :number
        (boolean? v) :boolean, :else :any))

;; Type keyword: :string, :number, :map, :seq, etc.
(register-schema-rule!
 #(when (keyword? %) {:type %}))

;; Record map: {:field1 type1 ...}
(register-schema-rule!
 #(when (map? %)
    {:type :map
     :child-schema (fn [k] (get % k :any))
     :valid-keys (when (every? keyword? (keys %)) (set (keys %)))}))

;; Enum (set): #{:a :b :c}
(register-schema-rule!
 #(when (set? %)
    (let [types (set (map infer-value-type %))]
      {:type (if (= 1 (count types)) (first types) :any)})))

;; Homogeneous seq: [element-type]
(register-schema-rule!
 #(when (and (vector? %) (= 1 (count %)))
    {:type :seq :child-schema (constantly (first %))}))
