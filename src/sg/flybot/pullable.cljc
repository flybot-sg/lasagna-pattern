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
  (:require [sg.flybot.pullable.core :as core]
            [clojure.walk :as walk]))

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
   | :pred    | (? :pred <pred> [<args>...])                    | Match if (pred args... val) truthy; pred can be fn/map/set |
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
   | :filter  | (? :filter <pred> [<sym>])                      | Filter elements by pred (fn/map/set) |
   | :first   | (? :first <pred> [<sym>])                       | Find first matching pred (fn/map/set)|
   | :sub     | (? :sub <ifn> [<matcher>])                      | Apply ifn to transform matched value |
   | :update  | (? :update <fn>)                                | Apply fn to value (use :var to bind) |

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

(defn transform
  "Match pattern against data and return {:result <updated-data> :vars <bindings>}.

   Use this when the pattern contains (? :update ...) or (? :sub ...) matchers
   and you want both the transformed data structure and the variable bindings.

   Returns nil if the pattern doesn't match.

   Pattern can be:
   - A CompiledMatcher (from `compile`) - efficient, no recompilation
   - A raw pattern (map, vector, list) - compiles on each call

   Examples:
     ;; Transform a single value
     (transform '(? :update inc) 5)
     ;=> {:result 6 :vars {}}

     ;; Shorthand: (?x :update fn) combines binding and update
     (transform '{:a (?x :update inc)} {:a 5})
     ;=> {:result {:a 6} :vars {x 6}}

     ;; Deep transformation in maps
     (transform '{:count (? :update inc)} {:count 5 :name \"test\"})
     ;=> {:result {:count 6 :name \"test\"} :vars {}}

     ;; Multiple updates
     (transform '{:a (? :update inc) :b {:c (? :update dec)}}
                {:a 1 :b {:c 10}})
     ;=> {:result {:a 2 :b {:c 9}} :vars {}}"
  [pattern data]
  (let [matcher (if (core/compiled-matcher? pattern)
                  pattern
                  (compile pattern))
        result (matcher data)]
    (when-not (core/failure? result)
      {:result (:val result)
       :vars (:vars result)})))

(defn transform!
  "Like transform but throws on match failure.

   Examples:
     (transform! '{:x (? :update inc)} {:x 1})
     ;=> {:result {:x 2} :vars {}}

     (transform! '{:x (? :pred even?)} {:x 3})
     ;=> throws ExceptionInfo"
  [pattern data]
  (let [matcher (if (core/compiled-matcher? pattern)
                  pattern
                  (compile pattern))
        result (matcher data)]
    (if (core/failure? result)
      (throw (ex-info (str "Transform failed: " (:reason result))
                      {:failure result
                       :pattern (if (core/compiled-matcher? pattern)
                                  (:pattern pattern)
                                  pattern)
                       :data data
                       :path (:path result)}))
      {:result (:val result)
       :vars (:vars result)})))

;; Re-export useful core functions

(def failure?
  "Returns true if x is a MatchFailure"
  core/failure?)

(def substitute-vars
  "Runtime substitution: walk `form` and replace ?x symbols with values from `vars` map.
   Works with dynamic patterns at runtime.

   Example:
     (substitute-vars '(+ ?x ?x) {'x 5}) ;=> (+ 5 5)"
  core/substitute-vars)

(defn rewrite-rule
  "Create a rewrite rule that matches `from-pattern` and produces `to-pattern`.

   Returns a function that:
   - Takes data and attempts to match against from-pattern
   - On match, substitutes bound variables into to-pattern
   - Returns the rewritten form, or nil if no match

   Patterns use vector syntax for sequences (matches lists and vectors).

   Examples:
     ;; Double to add: (* 2 x) -> (+ x x)
     (def double->add (rewrite-rule '[* 2 ?x] '(+ ?x ?x)))
     (double->add '(* 2 5))     ;=> (+ 5 5)
     (double->add '(* 2 y))     ;=> (+ y y)
     (double->add '(+ 1 2))     ;=> nil

     ;; Nested patterns work too
     (def combine (rewrite-rule '[* 2 [* 3 ?x]] '(* 6 ?x)))
     (combine '(* 2 (* 3 y)))   ;=> (* 6 y)"
  [from-pattern to-pattern]
  (let [matcher (compile from-pattern)]
    (fn [data]
      (when-let [vars (query matcher data)]
        (substitute-vars to-pattern vars)))))

;;=============================================================================
;; Pattern-based binding forms
;;=============================================================================

(defn- extract-var-symbols
  "Extract variable symbols from a pattern.
   Returns a set of symbols (without the ? prefix).
   E.g., '{:a ?a :b [?b0 ?_]} -> #{a b0}"
  [pattern]
  (let [vars (atom #{})]
    (walk/prewalk
     (fn [x]
       (when-let [[sym _] (core/named-var? x)]
         (when sym  ;; skip wildcards (?_)
           (swap! vars conj sym)))
       x)
     pattern)
    @vars))

(defmacro plet
  "Pattern-based let binding.

   Matches pattern against data, then evaluates body with bound variables.
   Variables in pattern use ?x syntax, body uses plain symbols (x, not ?x).

   Returns MatchFailure on match failure, otherwise the result of body.

   Examples:
     (plet [{:a ?a :b ?b} {:a 3 :b 4}] (* a b))
     ;=> 12

     (plet [{:a {:x ?x}} {:a {:x 5}}] x)
     ;=> 5

     (plet [{:a ?a} {:b 3}] a)
     ;=> #MatchFailure{...}"
  [[pattern data] & body]
  (let [var-syms (extract-var-symbols pattern)]
    `(let [result# ((compile '~pattern) ~data)]
       (if (core/failure? result#)
         result#
         (let [~'vars## (:vars result#)
               ~@(mapcat (fn [s] [s `(get ~'vars## '~s)]) var-syms)]
           ~@body)))))

(defmacro pfn
  "Pattern-based anonymous function.

   Creates a function that matches its argument against pattern,
   then substitutes bound variables into body and evaluates it.

   Both pattern and body use ?x syntax for variables.

   Returns MatchFailure on match failure, otherwise the evaluated result.

   Examples:
     (def f (pfn {:a ?a :b ?b} (+ ?a ?b)))
     (f {:a 3 :b 4})
     ;=> 7

     (def ex1 (pfn {:a ?a :b [?b0 ?_ ?b2]} [(* ?a ?b2) (+ ?a ?b0)]))
     (ex1 {:a 3 :b [2 0 4]})
     ;=> [12 5]"
  [pattern body]
  `(let [matcher# (compile '~pattern)
         subst-fn# (core/substitute '~body)]
     (fn [data#]
       (let [result# (matcher# data#)]
         (if (core/failure? result#)
           result#
           (subst-fn# result#))))))

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; compile - returns CompiledMatcher
  ;;-------------------------------------------------------------------
  (def m (compile '{:a ?a :b ?b}))

  ;; callable as function - returns ValMatchResult
  (m {:a 1 :b 2}) ;=>> {:vars '{a 1 b 2}}
  ;; ignores extra keys in data
  (m {:a 1 :b 2 :c 3}) ;=>> {:vars '{a 1 b 2}}

  ;;-------------------------------------------------------------------
  ;; query - returns vars map or nil
  ;;-------------------------------------------------------------------
  ;; succeeds - returns bindings
  (query m {:a 1 :b 2}) ;=>> '{a 1 b 2}
  ;; fails - returns nil
  (query '{:x 10} {:x 99}) ;=> nil
  ;; raw pattern (compiles each time - for REPL)
  (query '{:x ?x} {:x 42}) ;=>> '{x 42}

  ;;-------------------------------------------------------------------
  ;; match-result returns vars or MatchFailure
  ;;-------------------------------------------------------------------
  ;; succeeds, returns vars
  (match-result m {:a 1 :b 2}) ;=>> {:vars '{a 1 b 2}}
  ;; fails - returns MatchFailure with diagnostic path
  (match-result '{:x 10} {:x 99}) ;=>> {:reason #"value mismatch" :path [:x]}

  ;;-------------------------------------------------------------------
  ;; match! - returns vars or throws
  ;;-------------------------------------------------------------------
  (match! m {:a 1 :b 2}) ;=>> '{a 1 b 2}

  ;;-------------------------------------------------------------------
  ;; Sequence patterns
  ;;-------------------------------------------------------------------
  ;; fixed-length sequence
  ((compile '[?a ?b]) [1 2]) ;=>> {:vars '{a 1 b 2}}
  ;; head + rest pattern
  (query '[?head ?tail+] [1 2 3]) ;=>> '{head 1 tail (2 3)}

  ;;-------------------------------------------------------------------
  ;; Nested patterns
  ;;-------------------------------------------------------------------
  ;; nested sequence matching
  (query '[[?x]] '((inner))) ;=>> '{x inner}
  (query '[?a [?b ?c]] '(x (y z))) ;=>> '{a x b y c z}
  ;; deeply nested
  (query '[?op [?nested ?x ?y] ?z] '(+ (* a b) c)) ;=>> '{op + nested * x a y b z c}

  ;;-------------------------------------------------------------------
  ;; rewrite-rule - pattern-based term rewriting
  ;;-------------------------------------------------------------------
  ;; basic rewrite: (* 2 x) -> (+ x x)
  (def double->add (rewrite-rule '[* 2 ?x] '(+ ?x ?x)))
  (double->add '(* 2 5)) ;=>> '(+ 5 5)
  (double->add '(* 2 y)) ;=>> '(+ y y)
  ;; no match returns nil
  (double->add '(+ 1 2)) ;=> nil
  ;; nested rewrite: (* 2 (* 3 x)) -> (* 6 x)
  (def combine-mults (rewrite-rule '[* 2 [* 3 ?x]] '(* 6 ?x)))
  (combine-mults '(* 2 (* 3 y))) ;=>> '(* 6 y)
  (combine-mults '(* 2 (* 3 (+ a b)))) ;=>> '(* 6 (+ a b))
  ;; rewrite with multiple variables
  (def swap-args (rewrite-rule '[f ?a ?b] '(f ?b ?a)))
  (swap-args '(f 1 2)) ;=>> '(f 2 1)

  ;;-------------------------------------------------------------------
  ;; transform - returns both updated data and bindings
  ;;-------------------------------------------------------------------
  ;; basic update: transforms a single value
  (transform '(? :update inc) 5)
  ;=>> {:result 6 :vars {}}

  ;; compose with :var to bind result (verbose form)
  (transform '(? :var n (? :update inc)) 5)
  ;=>> {:result 6 :vars {'n 6}}

  ;; shorthand: (?x :update fn) combines binding and update
  (transform '(?n :update inc) 5)
  ;=>> {:result 6 :vars {'n 6}}

  ;; shorthand in maps
  (transform '{:a (?x :update inc)} {:a 5})
  ;=>> {:result {:a 6} :vars {'x 6}}

  ;; deep map update: reconstructs nested structure
  (transform '{:a {:b (? :update inc)}} {:a {:b 5} :c 10})
  ;=>> {:result {:a {:b 6} :c 10} :vars {}}

  ;; multiple updates: transforms at multiple paths
  (transform '{:a (? :update inc) :b (? :update dec)} {:a 1 :b 10})
  ;=>> {:result {:a 2 :b 9} :vars {}}

  ;; sequence update: transforms elements in place
  (transform '[(? :update inc) ?rest*] [1 2 3])
  ;=>> {:result [2 2 3] :vars {'rest '(2 3)}}

  ;; transform returns nil on mismatch
  (transform '{:x (? :pred even?)} {:x 3})
  ;=> nil

  ;;-------------------------------------------------------------------
  ;; extract-var-symbols - helper for plet
  ;;-------------------------------------------------------------------
  ;; extracts var names from pattern (without ? prefix)
  (extract-var-symbols '{:a ?a :b ?b}) ;=>> #{'a 'b}
  ;; handles nested patterns
  (extract-var-symbols '{:a ?a :b {:c ?c}}) ;=>> #{'a 'c}
  ;; handles sequence patterns
  (extract-var-symbols '[?x ?y ?z]) ;=>> #{'x 'y 'z}
  ;; skips wildcards
  (extract-var-symbols '[?a ?_ ?b]) ;=>> #{'a 'b}
  ;; handles quantifiers
  (extract-var-symbols '[?head ?tail+]) ;=>> #{'head 'tail}

  ;;-------------------------------------------------------------------
  ;; plet - pattern-based let binding
  ;;-------------------------------------------------------------------
  ;; basic map destructuring
  (plet [{:a ?a :b ?b} {:a 3 :b 4}] (* a b)) ;=> 12
  ;; nested pattern
  (plet [{:a {:x ?x}} {:a {:x 5}}] x) ;=> 5
  ;; sequence pattern
  (plet [[?first ?second] [10 20]] (+ first second)) ;=> 30
  ;; match failure returns MatchFailure (use exact value to force failure)
  (plet [{:a 1} {:a 2}] :never-reached) ;=>> failure?
  ;; multiple expressions in body
  (plet [{:x ?x} {:x 5}] (let [y 10] (+ x y))) ;=> 15

  ;;-------------------------------------------------------------------
  ;; pfn - pattern-based function
  ;;-------------------------------------------------------------------
  ;; basic usage
  (let [f (pfn {:a ?a :b ?b} (+ ?a ?b))]
    (f {:a 3 :b 4})) ;=> 7
  ;; with sequence pattern and vector result
  (let [ex1 (pfn {:a ?a :b [?b0 ?_ ?b2]} [(* ?a ?b2) (+ ?a ?b0)])]
    (ex1 {:a 3 :b [2 0 4]})) ;=> [12 5]
  ;; match failure returns MatchFailure (use exact value to force failure)
  (let [f (pfn {:a 1} :matched)]
    (f {:a 2})) ;=>> failure?
  ;; nested pattern
  (let [f (pfn {:outer {:inner ?x}} (* ?x ?x))]
    (f {:outer {:inner 7}})) ;=> 49
  )
