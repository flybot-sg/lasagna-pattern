(ns sg.flybot.pullable
  "Public API for the pullable pattern matching library."
  (:require [sg.flybot.pullable.core :as core]))

;;=============================================================================
;; Core API
;;=============================================================================

(def failure?
  "Returns true if x is a MatchFailure.

   MatchFailure is returned when pattern matching fails. It contains:
   - :reason       - Human-readable failure message
   - :matcher-type - Keyword identifying the matcher (e.g., :pred, :map)
   - :path         - Vector of keys/indices showing location in data
   - :value        - The value that failed to match
   - :depth        - Integer tracking match progress (for nested failures)

   Example:
     (let [f (match-fn {:a ?x} ?x)
           result (f \"not a map\")]
       (when (failure? result)
         (println \"Failed at path:\" (:path result))
         (println \"Reason:\" (:reason result))))"
  core/failure?)

(defmacro match-fn
  "Create a function that pattern-matches its argument and evaluates body.

   On successful match, returns the result of evaluating body with pattern
   variables bound. On failed match, returns a MatchFailure record.

   The special symbol $ is bound to the matched/transformed value.

   ## Pattern Syntax

   | Pattern  | Description                              |
   |----------|------------------------------------------|
   | ?x       | Bind value to symbol ?x                  |
   | _        | Wildcard (match anything, no binding)    |
   | ?x?      | Optional element (0 or 1)                |
   | ?x+      | One or more elements (lazy)              |
   | ?x*      | Zero or more elements (lazy)             |
   | ?x+!     | One or more (greedy)                     |
   | ?x*!     | Zero or more (greedy)                    |

   ## Examples

     ;; Simple binding
     ((match-fn ?x (+ ?x 2)) 3)  ;=> 5

     ;; Map destructuring
     ((match-fn {:a ?a :b ?b} (+ ?a ?b)) {:a 1 :b 2})  ;=> 3

     ;; $ binds to matched value
     ((match-fn ?x (inc $)) 42)  ;=> 43

     ;; $ with map gets full map
     ((match-fn {:a ?x} (assoc $ :result ?x)) {:a 1 :b 2})
     ;=> {:a 1 :b 2 :result 1}

     ;; Sequence patterns
     ((match-fn [?a ?b ?c] (+ ?a ?b ?c)) [1 2 3])  ;=> 6

     ;; String operations
     ((match-fn {:name ?n} (str \"Hello, \" ?n \"!\")) {:name \"Alice\"})
     ;=> \"Hello, Alice!\"

   ## Handling Failures

   Use `failure?` to check for match failures, then access MatchFailure fields:

     (let [f (match-fn {:x ?x} ?x)
           result (f \"not a map\")]
       (if (failure? result)
         {:error (:reason result)
          :at    (:path result)}
         result))"
  [pattern body]
  `(core/match-fn ~pattern ~body))
