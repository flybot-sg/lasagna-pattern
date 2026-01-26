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

(defn infer-schema-type
  "Infer the type keyword from a schema value."
  [schema]
  (:type (get-schema-info schema)))

;;=============================================================================
;; Schema Validation
;;=============================================================================

(def ^:private subseq-pattern-types
  "Internal pattern types for sequence matching - always valid"
  #{:one :repeat :optional :term :subseq})

(defn valid-pattern-for-schema?
  "Check if pattern type is valid for schema type."
  [pattern-type schema-type]
  (or (subseq-pattern-types pattern-type)
      (case schema-type
        :any true
        :map (#{:map :var :any} pattern-type)
        :seq (#{:seq :var :any} pattern-type)
        (:number :string :keyword :symbol :boolean)
        (#{:var :val :pred :any :regex} pattern-type)
        true)))

(defn validate-pattern-schema!
  "Validate pattern against schema. Throws on violation.
   core-pattern? checks if x is a (? :type ...) pattern."
  [ptn schema core-pattern?]
  (when (and schema (core-pattern? ptn))
    (let [ptn-type (second ptn)
          {:keys [type child-schema valid-keys]} (get-schema-info schema)]
      (when-not (valid-pattern-for-schema? ptn-type type)
        (throw (ex-info (str "Schema violation: pattern :" ptn-type " vs schema :" type)
                        {:pattern-type ptn-type :schema-type type :schema schema})))
      (case ptn-type
        :map (doseq [[k child] (partition 2 (drop 2 ptn))]
               (when (and valid-keys (not (valid-keys k)))
                 (throw (ex-info (str "Key " k " not in schema") {:key k :schema-keys valid-keys})))
               (when child-schema
                 (validate-pattern-schema! child (child-schema k) core-pattern?)))
        :seq (when child-schema
               (doseq [[i child] (map-indexed vector (drop 2 ptn))]
                 (validate-pattern-schema! child (child-schema i) core-pattern?)))
        (doseq [child (drop 2 ptn)]
          (when (core-pattern? child)
            (validate-pattern-schema! child schema core-pattern?)))))))

;;=============================================================================
;; Schema Filtering
;;=============================================================================

(defn filter-by-schema
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

(defn wrap-with-schema-filter
  "Wrap matcher to filter :val by schema. failure? checks for MatchFailure."
  [matcher schema failure?]
  (fn [mr]
    (let [result (matcher mr)]
      (if (failure? result) result (update result :val filter-by-schema schema)))))

;;=============================================================================
;; Built-in Schema Rules
;;=============================================================================

(defn infer-value-type
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
