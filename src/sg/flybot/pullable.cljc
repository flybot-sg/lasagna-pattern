(ns sg.flybot.pullable
  "Public API for the pullable pattern matching library."
  (:require [sg.flybot.pullable.core :as core]))

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
  core/failure?)

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
  core/register-var-option!)

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

   Special binding: $ is bound to the matched/transformed value.

   Examples:
     ((match-fn {:a ?a :b ?b} (+ ?a ?b)) {:a 1 :b 2})  ;=> 3
     ((match-fn [?first ?rest*] ?rest) [1 2 3])        ;=> (2 3)
     ((match-fn {:a ?x} (assoc $ :sum ?x)) {:a 1 :b 2});=> {:a 1 :b 2 :sum 1}

   Failure handling:
     (let [result ((match-fn {:x ?x} ?x) \"bad\")]
       (when (failure? result)
         (:reason result)))  ; human-readable error"
  [pattern body]
  `(core/match-fn ~pattern ~body))

;;=============================================================================
;; Rule-based Transformation
;;=============================================================================

(defmacro rule
  "Create a transformation rule: pattern → template.

   Returns a function that:
   - On match: returns template with ?vars substituted
   - On no match: returns nil

   Can be used for:
   1. Runtime data transformation
   2. Compile-time pattern rewriting (via :rules option in compile-pattern)

   Examples:
     ;; Algebraic simplification
     (def double-to-add (rule (* 2 ?x) (+ ?x ?x)))
     (double-to-add '(* 2 5))  ;=> (+ 5 5)
     (double-to-add '(* 3 5))  ;=> nil

     ;; Custom pattern syntax
     (def not-nil (rule (not-nil ?x) (?x :when some?)))
     (compile-pattern '{:name (not-nil ?n)} {:rules [not-nil]})"
  [pattern template]
  `(core/rule ~pattern ~template))

(def compile-pattern
  "Compile a pattern to a matcher function.

   Two-phase compilation:
   1. Rewrite: Transform syntax sugar to core patterns
   2. Compile: Build matcher functions

   Options (optional second argument):
     :rules - additional rewrite rules prepended to defaults
     :only  - use only these rules, ignoring defaults

   Examples:
     (compile-pattern '{:name ?n})
     ; Returns a matcher

     ;; With custom rules
     (def not-nil (rule (not-nil ?x) (?x :when some?)))
     (compile-pattern '{:name (not-nil ?n)} {:rules [not-nil]})"
  core/compile-pattern)

(def apply-rules
  "Apply rules recursively throughout a data structure (bottom-up).
   Each node is transformed by the first matching rule.

   Example:
     (def simplify-mul-2 (rule (* 2 ?x) (+ ?x ?x)))
     (def simplify-add-0 (rule (+ 0 ?x) ?x))

     (apply-rules [simplify-mul-2 simplify-add-0]
                  '(+ 0 (* 2 y)))
     ;=> (+ y y)"
  core/apply-rules)
