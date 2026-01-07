(ns sg.flybot.pullable
  "Public API for the pullable pattern matching library.

   ## Quick Start

   ```clojure
   (require '[sg.flybot.pullable :as p])

   ;; Compile a pattern (do this once, reuse many times)
   (def m (p/compile '{:name ?name :age ?age}))

   ;; Query for variable bindings (most common)
   (p/query m {:name \"Alice\" :age 30})
   ;=> {name \"Alice\" age 30}

   ;; Or call directly for raw result
   (m {:name \"Alice\" :age 30})
   ;=> #ValMatchResult{:val {...} :vars {name \"Alice\" age 30}}

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

(defn compile
  "Compile a pattern into a CompiledMatcher.

   The returned matcher can be used in multiple ways:

   1. As a function (returns raw ValMatchResult or MatchFailure):
      (m data) ;=> ValMatchResult or MatchFailure

   2. With `query` (returns vars map or nil):
      (query m data) ;=> {name \"Alice\"} or nil

   3. With `match-result` (returns {:vars ...} or MatchFailure):
      (match-result m data) ;=> {:vars {...}} or MatchFailure

   4. With `match!` (returns vars map or throws):
      (match! m data) ;=> {name \"Alice\"} or throws

   Optional rules argument adds custom rules that take precedence over core-rules.
   Core-rules are always applied as fallback.

   ## Pattern Syntax

   Maps and vectors are matched structurally. Literals match exact values.
   Special forms use (? :type args...) syntax.

   ### Variable Bindings (shorthand)

   | Pattern  | Description                              |
   |----------|------------------------------------------|
   | ?x       | Bind value to 'x                         |
   | ?_       | Wildcard (match anything, no binding)    |
   | ?x?      | Optional element (0 or 1)                |
   | ?x+      | One or more elements (lazy)              |
   | ?x*      | Zero or more elements (lazy)             |
   | ?x+!     | One or more elements (greedy)            |
   | ?x*!     | Zero or more elements (greedy)           |

   ### Core Matchers

   | Type     | Syntax                                          | Description                          |
   |----------|-------------------------------------------------|--------------------------------------|
   | :pred    | (? :pred <fn> [<args>...])                      | Match if (fn args... val) is truthy  |
   | :val     | (? :val <value>)                                | Match exact value                    |
   | :map     | (? :map <map>)                                  | Match map structure                  |
   | :seq     | (? :seq [<m>...] [:min <n>] [:max <n>] [:as <sym>]) | Match sequence, optionally repeated |
   | :var     | (? :var <sym> <matcher>)                        | Bind match result to variable        |
   | :one     | (? :one <matcher>)                              | Match single element in sequence     |
   | :optional| (? :optional <matcher>)                         | Optional element in sequence         |
   | :repeat  | (? :repeat <m> :min <n> [:max <n>] [:as <sym>]) | Repeat matcher min to max times      |
   | :or      | (? :or <matcher>...)                            | First successful alternative wins    |
   | :not     | (? :not <matcher>)                              | Succeed if child matcher fails       |
   | :->      | (? :-> <matcher>...)                            | Chain matchers sequentially          |
   | :match-case | (? :match-case [<key> <matcher>...] [<sym>]) | Match first case, bind key to sym    |
   | :filter  | (? :filter <pred> [<sym>])                      | Filter sequence elements by pred     |
   | :first   | (? :first <pred> [<sym>])                       | Find first element matching pred     |
   | :sub     | (? :sub <fn> [<matcher>])                       | Apply fn to transform matched value  |

   ### Variable References in Args

   Use $var syntax in :pred args to reference previously bound variables:
     (? :pred < $a)  ; calls (< <value-of-a> current-val)

   ## Examples

     (def m (compile '{:name ?name :age ?age}))

     ;; As function - raw result
     (m {:name \"Alice\" :age 30})
     ;=> #ValMatchResult{:val {:name \"Alice\" :age 30} :vars {name \"Alice\" age 30}}

     ;; Query - just vars or nil
     (query m {:name \"Alice\" :age 30})
     ;=> {name \"Alice\" age 30}

     ;; Sequence with head and tail
     (compile '[?head ?tail+])

     ;; Predicate matching
     (compile '(? :pred even?))

     ;; Alternatives
     (compile '(? :or (? :pred even?) (? :val -1)))"
  ([pattern]
   (compile pattern []))
  ([pattern rules]
   (let [all-rules (into (vec rules) core/core-rules)
         matcher-fn (core/ptn->matcher pattern all-rules)]
     (core/compiled-matcher matcher-fn pattern))))

;; Protocol functions - work on CompiledMatcher or raw patterns

(defn query
  "Match pattern against data and return variable bindings, or nil if no match.

   Pattern can be:
   - A CompiledMatcher (from `compile`) - efficient, no recompilation
   - A raw pattern (map, vector, list) - compiles on each call, for REPL use

   Examples:
     (def m (compile '{:a ?a}))
     (query m {:a 1})        ;=> {a 1}
     (query m {:b 1})        ;=> nil

     ;; Raw pattern (compiles each time)
     (query '{:a ?a} {:a 1}) ;=> {a 1}"
  [pattern data]
  (core/query pattern data))

(defn match-result
  "Match pattern against data. Returns {:vars ...} on success or MatchFailure on failure.

   Use this when you need failure diagnostics (reason, path, etc.).

   Examples:
     (def m (compile '{:a ?a}))
     (match-result m {:a 1})  ;=> {:vars {a 1}}
     (match-result m {:b 1})  ;=> #MatchFailure{:reason \"...\" :path [:a] ...}"
  [pattern data]
  (core/match-result pattern data))

(defn match!
  "Match pattern against data. Returns vars map on success, throws on failure.

   Use this in pipelines or when failure should be exceptional.

   Examples:
     (def m (compile '{:a ?a}))
     (match! m {:a 1})  ;=> {a 1}
     (match! m {:b 1})  ;=> throws ExceptionInfo with failure details"
  [pattern data]
  (core/match! pattern data))

;; Re-export useful core functions

(def failure?
  "Returns true if x is a MatchFailure"
  core/failure?)

^:rct/test
(comment
  ;; Compile returns a CompiledMatcher
  (def m (compile '{:a ?a :b ?b}))

  ;; As function - returns raw ValMatchResult
  (m {:a 1 :b 2}) ;=>>
  {:vars '{a 1 b 2}}
  (m {:a 1 :b 2 :c 3}) ;=>>
  {:vars '{a 1 b 2}}

  ;; query - returns vars map or nil
  (query m {:a 1 :b 2}) ;=>>
  '{a 1 b 2}

  ;; query with raw pattern (compiles each time)
  (query '{:x ?x} {:x 42}) ;=>>
  '{x 42}

  ;; match-result - returns {:vars ...} or MatchFailure
  (match-result m {:a 1 :b 2}) ;=>>
  {:vars '{a 1 b 2}}

  ;; match! - returns vars or throws
  (match! m {:a 1 :b 2}) ;=>>
  '{a 1 b 2}

  ;; failure returns MatchFailure with diagnostic info
  ((compile '{:x 10}) {:x 99}) ;=>>
  {:reason #"value mismatch" :path [:x]}

  ;; query returns nil on value mismatch
  (query '{:x 10} {:x 99}) ;=>
  nil

  ;; match-result returns MatchFailure on value mismatch
  (match-result '{:x 10} {:x 99}) ;=>>
  {:reason #"value mismatch" :path [:x]}

  ;; sequence pattern
  ((compile '[?a ?b]) [1 2]) ;=>>
  {:vars '{a 1 b 2}}
  (query '[?head ?tail+] [1 2 3]) ;=>>
  '{head 1 tail (2 3)})
