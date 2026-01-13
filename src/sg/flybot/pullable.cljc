(ns sg.flybot.pullable
  "Public API for the pullable pattern matching library.

   ## Quick Start

   ```clojure
   (require '[sg.flybot.pullable :as p])

   ;; Compile a pattern (do this once, reuse many times)
   (def m (p/compile '{:name name :age age}))

   ;; Query for variable bindings (most common)
   (p/query m {:name \"Alice\" :age 30})
   ;=> {name \"Alice\" age 30}

   ;; Get failure details
   (p/match-result m {:name \"Alice\"})
   ;=> #MatchFailure{:reason \"...\" :path [:age] ...}

   ;; Throw on failure
   (p/match! m {:name \"Alice\" :age 30})
   ;=> {name \"Alice\" age 30}
   ```

   ## Pattern Syntax

   See `compile` docstring for full pattern syntax reference."
  (:refer-clojure :exclude [compile])
  (:require [sg.flybot.pullable.core :as core]))

;;=============================================================================
;; Core API - compile, query, match-result, match!
;;=============================================================================

(defn compile
  "Compile a pattern into a matcher function.

   Returns a function that takes data and returns a ValMatchResult or MatchFailure.

   ## Pattern Syntax

   Maps and vectors are matched structurally. Literals match exact values.
   Special forms use (? :type args...) syntax.

   ### Variable Bindings

   | Pattern  | Description                              |
   |----------|------------------------------------------|
   | x        | Bind value to 'x (plain symbol)          |
   | _        | Wildcard (match anything, no binding)    |
   | x?       | Optional element (0 or 1)                |
   | x+       | One or more elements (lazy)              |
   | x*       | Zero or more elements (lazy)             |
   | x+!      | One or more elements (greedy)            |
   | x*!      | Zero or more elements (greedy)           |

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
   | :match-case | (? :match-case <key> <matcher>... [<sym>])      |
   | :filter     | (? :filter <pred> [<sym>])                      |
   | :first      | (? :first <pred> [<sym>])                       |
   | :sub        | (? :sub [<matcher>] <ifn>)                      |
   | :update     | (? :update <fn>)                                |

   ## Examples

     (def m (compile '{:name name :age age}))
     (m {:name \"Alice\" :age 30})
     ;=> #ValMatchResult{:val {...} :vars {name \"Alice\" age 30}}

     (query m {:name \"Alice\" :age 30})
     ;=> {name \"Alice\" age 30}"
  ([pattern]
   (compile pattern []))
  ([pattern _rules]
   ;; Note: custom rules are not currently supported in the two-phase compilation
   (let [matcher-fn (core/ptn->matcher pattern)]
     (fn [data]
       (matcher-fn (core/vmr data))))))

^:rct/test
(comment
  ;; simple symbol binding
  ((compile 'x) 3) ;=>> {:vars {'x 3}}
  ;; nested structure with wildcards
  ((compile '[1 _ 3 x [5 y]]) [1 2 3 4 [5 6]])) ;=>> {:vars '{x 4 y 6}})
