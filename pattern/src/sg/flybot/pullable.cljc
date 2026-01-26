(ns sg.flybot.pullable
  "Public API for the pullable pattern matching library."
  (:require [sg.flybot.pullable.impl :as impl]))

;;=============================================================================
;; Core API
;;=============================================================================

(def failure?
  "Predicate for MatchFailure records.

   MatchFailure fields:
   - :reason  - Human-readable message
   - :path    - Vector of keys/indices to failure location
   - :value   - The value that failed

   Example:
     (let [result ((match-fn {:a ?x} ?x) \"not a map\")]
       (when (failure? result)
         (:reason result)))  ;=> \"expected map, got ...\""
  impl/failure?)

(def register-var-option!
  "Register a custom option handler for extended matching variables.

   Extended vars have syntax: (?x :opt1 val1 :opt2 val2 ...)
   Each option handler receives the option value and returns a pattern to chain.

   Built-in options:
   - :when <pred>    - Add predicate check
   - :default <val>  - Substitute default for nil

   Example - register a :transform option:
     (register-var-option! :transform
       (fn [f] (list '? :sub f)))

     ;; Usage: (?x :transform str/upper-case)
     ((match-fn {:name (?n :transform clojure.string/upper-case)} ?n)
      {:name \"alice\"})  ;=> \"ALICE\""
  impl/register-var-option!)

(def register-schema-rule!
  "Register a rule for schema type inference and validation.

   Rule is a function: (schema) -> {:type t, :child-schema fn, :valid-keys set} | nil

   Return map fields:
   - :type        - Schema type keyword (:map, :seq, :string, :number, etc.)
   - :child-schema - (optional) fn: (key-or-index) -> sub-schema for children
   - :valid-keys   - (optional) set of allowed keys (for record schemas)

   Use match-fn to define rules that pattern-match the schema structure.
   Rules are tried in reverse order (last registered wins).

   Example - register a :non-empty seq schema:
     (register-schema-rule!
       (match-fn [:non-empty ?elem-type]
                 {:type :seq
                  :child-schema (constantly ?elem-type)}))

     ;; Usage: [:non-empty :number] validates sequences of numbers"
  impl/register-schema-rule!)

(def get-schema-info
  "Get type and child accessor from a schema using registered rules.

   Returns a map with:
   - :type        - Schema type keyword (:map, :seq, :string, :number, etc.)
   - :child-schema - (optional) fn: (key-or-index) -> sub-schema for children
   - :valid-keys   - (optional) set of allowed keys (for record schemas)

   Example:
     (get-schema-info {:name :string :age :number})
     ;=> {:type :map,
     ;    :child-schema fn,
     ;    :valid-keys #{:name :age}}"
  impl/get-schema-info)

(defmacro match-fn
  "Create a pattern-matching function. Returns body result on match, MatchFailure on failure.

   Pattern syntax:
     ?x       - Bind to symbol x
     ?_       - Wildcard (no binding)
     ?x?      - Optional (0-1)
     ?x* ?x+  - Zero/one-or-more (lazy)
     ?x*! ?x+!- Zero/one-or-more (greedy)
     {}       - Map pattern
     []       - Sequence pattern

   Compile options (optional third argument):
     :rules   - custom rewrite rules (prepended to defaults)
     :only    - use only these rules, ignoring defaults
     :schema  - schema for compile-time validation
     :resolve - custom symbol resolver (fn [sym] -> value)
     :eval    - custom form evaluator (fn [form] -> value)

   Special binding: $ is bound to the matched/transformed value.

   Examples:
     ((match-fn {:a ?a :b ?b} (+ ?a ?b)) {:a 1 :b 2})  ;=> 3
     ((match-fn [?first ?rest*] ?rest) [1 2 3])        ;=> (2 3)
     ((match-fn {:a ?x} (assoc $ :sum ?x)) {:a 1 :b 2});=> {:a 1 :b 2 :sum 1}

     ;; With schema validation
     ((match-fn {:name ?n} ?n {:schema {:name :string}}) {:name \"alice\"}) ;=> \"alice\"

   Failure handling:
     (let [result ((match-fn {:x ?x} ?x) \"bad\")]
       (when (failure? result)
         (:reason result)))  ; human-readable error"
  ([pattern body] `(impl/match-fn ~pattern ~body))
  ([pattern body opts] `(impl/match-fn ~pattern ~body ~opts)))

;;=============================================================================
;; Rule-based Transformation
;;=============================================================================

(defmacro rule
  "Create a transformation rule: pattern → template.

   Returns a function that:
   - On match: returns template with ?vars substituted
   - On no match: returns nil

   Examples:
     ;; Algebraic simplification
     (def double-to-add (rule (* 2 ?x) (+ ?x ?x)))
     (double-to-add '(* 2 5))  ;=> (+ 5 5)
     (double-to-add '(* 3 5))  ;=> nil"
  [pattern template]
  `(impl/rule ~pattern ~template))

(def apply-rules
  "Apply rules recursively throughout a data structure (bottom-up).
   Each node is transformed by the first matching rule.

   Example:
     (def simplify-mul-2 (rule (* 2 ?x) (+ ?x ?x)))
     (def simplify-add-0 (rule (+ 0 ?x) ?x))

     (apply-rules [simplify-mul-2 simplify-add-0]
                  '(+ 0 (* 2 y)))
     ;=> (+ y y)"
  impl/apply-rules)
