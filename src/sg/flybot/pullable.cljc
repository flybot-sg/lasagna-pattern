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

(defn compile
  "Compile a pattern into a matcher function.

   Returns a function that takes data and returns a ValMatchResult or MatchFailure.

   ## Pattern Syntax

   Maps and vectors are matched structurally. Literals match exact values.
   Plain symbols match literal symbol values.
   Special forms use (? :type args...) syntax.

   ### Variable Bindings

   | Pattern  | Description                              |
   |----------|------------------------------------------|
   | ?x       | Bind value to '?x (must have ? prefix)   |
   | _        | Wildcard (match anything, no binding)    |
   | ?x?      | Optional element (0 or 1)                |
   | ?x+      | One or more elements (lazy)              |
   | ?x*      | Zero or more elements (lazy)             |
   | ?x+!     | One or more elements (greedy)            |
   | ?x*!     | Zero or more elements (greedy)           |

   ### Core Matchers

   | Type        | Syntax                                          |
   |-------------|-------------------------------------------------|
   | :pred       | (? :pred <pred>)                                |
   | :val        | (? :val <value>)                                |
   | :map        | (? :map <map>)                                  |
   | :seq        | (? :seq [<m>...] [:min <n>] [:max <n>])         |
   | :var        | (? :var <sym> <matcher>)                        |
   | :or         | (? :or <matcher>...)                            |
   | :not        | (? :not <matcher>)                              |
   | :->         | (? :-> <matcher>...)                            |
   | :case       | (? :case [<sym>] <key> <matcher>...)            |
   | :filter     | (? :filter <pred> [<sym>])                      |
   | :first      | (? :first <pred> [<sym>])                       |
   | :sub        | (? :sub [<matcher>] <ifn>)                      |
   | :update     | (? :update <fn>)                                |

   ## Examples

     (def m (compile '{:name ?name :age ?age}))
     (m {:name \"Alice\" :age 30})
     ;=> #ValMatchResult{:val {...} :vars {?name \"Alice\" ?age 30}}

     (query m {:name \"Alice\" :age 30})
     ;=> {?name \"Alice\" ?age 30}"
  ([pattern]
   (compile pattern []))
  ([pattern _rules]
   ;; Note: custom rules are not currently supported in the two-phase compilation
   (let [matcher-fn (core/ptn->matcher pattern)]
     (fn [data]
       (matcher-fn (core/vmr data))))))

^:rct/test
(comment
  ;; simple symbol binding (using ?-prefix)
  ((compile '?x) 3) ;=>> {:vars {'?x 3}}
  ;; nested structure with wildcards
  ((compile '[1 _ 3 ?x [5 ?y]]) [1 2 3 4 [5 6]]) ;=>> {:vars '{?x 4 ?y 6}}
  ;; plain symbols match literal values
  ((compile 'foo) 'foo) ;=>>
  {:val 'foo})

