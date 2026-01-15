(ns sg.flybot.pullable
  "Public API for the pullable pattern matching library. "
  (:refer-clojure :exclude [compile])
  (:require [sg.flybot.pullable.core :as core]))

;;=============================================================================
;; Core API
;;=============================================================================

(def failure?
  "Returns true if x is a MatchFailure.

   MatchFailure is returned when pattern matching fails. It contains:
   - :reason     - Human-readable failure message
   - :matcher-type - Keyword identifying the matcher (e.g., :pred, :map)
   - :path       - Vector of keys/indices showing location in data
   - :value      - The value that failed to match
   - :depth      - Integer tracking match progress

   Example:
     (let [result ((compile '{:a ?x}) \"not a map\")]
       (if (failure? result)
         (println \"Match failed:\" (:reason result))
         (:vars result)))"
  core/failure?)

(defmacro qfn
  "Query function - creates a function that pattern matches its argument
   and evaluates body with matched variables bound.

   Returns MatchFailure if pattern doesn't match.
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
     ((qfn ?x (+ ?x 2)) 3)  ;=> 5

     ;; Map destructuring
     ((qfn {:a ?a :b ?b} (+ ?a ?b)) {:a 1 :b 2})  ;=> 3

     ;; $ binds to matched value
     ((qfn ?x (inc $)) 42)  ;=> 43

     ;; $ with map gets full map
     ((qfn {:a ?x} (assoc $ :result ?x)) {:a 1 :b 2})
     ;=> {:a 1 :b 2 :result 1}

     ;; Sequence patterns
     ((qfn [?a ?b ?c] (+ ?a ?b ?c)) [1 2 3])  ;=> 6

     ;; Returns MatchFailure on mismatch
     (failure? ((qfn {:a ?x} ?x) \"not a map\"))  ;=> true

     ;; String operations
     ((qfn {:name ?n} (str \"Hello, \" ?n \"!\")) {:name \"Alice\"})
     ;=> \"Hello, Alice!\""
  [pattern body]
  `(core/qfn ~pattern ~body))
