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
  ([pattern rules]
   (let [all-rules (into (vec rules) core/core-rules)
         matcher-fn (core/ptn->matcher pattern all-rules)]
     (fn [data]
       (matcher-fn (core/vmr data))))))

(defn query
  "Match pattern against data and return variable bindings, or nil if no match.

   Pattern can be:
   - A compiled matcher (from `compile`) - efficient, no recompilation
   - A raw pattern (map, vector, list) - compiles on each call, for REPL use

   Examples:
     (def m (compile '{:a a}))
     (query m {:a 1})        ;=> {a 1}
     (query m {:b 1})        ;=> nil

     ;; Raw pattern (compiles each time)
     (query '{:a a} {:a 1})  ;=> {a 1}"
  [pattern data]
  (let [matcher (if (fn? pattern) pattern (compile pattern))
        result (matcher data)]
    (when-not (core/failure? result)
      (:vars result))))

(defn match-result
  "Match pattern against data. Returns {:vars ...} on success or MatchFailure on failure.

   Use this when you need failure diagnostics (reason, path, etc.).

   Examples:
     (def m (compile '{:a a}))
     (match-result m {:a 1})  ;=> {:vars {a 1}}
     (match-result m {:b 1})  ;=> #MatchFailure{:reason \"...\" :path [:a] ...}"
  [pattern data]
  (let [matcher (if (fn? pattern) pattern (compile pattern))
        result (matcher data)]
    (if (core/failure? result)
      result
      {:vars (:vars result)})))

(defn match!
  "Match pattern against data. Returns vars map on success, throws on failure.

   Use this in pipelines or when failure should be exceptional.

   Examples:
     (def m (compile '{:a a}))
     (match! m {:a 1})  ;=> {a 1}
     (match! m {:b 1})  ;=> throws ExceptionInfo with failure details"
  [pattern data]
  (let [matcher (if (fn? pattern) pattern (compile pattern))
        result (matcher data)]
    (if (core/failure? result)
      (throw (ex-info (str "Match failed: " (:reason result))
                      {:failure result
                       :pattern pattern
                       :data data
                       :path (:path result)}))
      (:vars result))))

;;=============================================================================
;; Transform API
;;=============================================================================

(defn transform
  "Match pattern against data and return {:result <updated-data> :vars <bindings>}.

   Use this when the pattern contains (? :update ...) or (? :sub ...) matchers
   and you want both the transformed data structure and the variable bindings.

   Returns nil if the pattern doesn't match.

   Examples:
     (transform '(? :update inc) 5)
     ;=> {:result 6 :vars {}}

     (transform '{:a (x :update inc)} {:a 5})
     ;=> {:result {:a 6} :vars {x 6}}"
  [pattern data]
  (let [matcher (if (fn? pattern) pattern (compile pattern))
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
  (let [matcher (if (fn? pattern) pattern (compile pattern))
        result (matcher data)]
    (if (core/failure? result)
      (throw (ex-info (str "Transform failed: " (:reason result))
                      {:failure result
                       :pattern pattern
                       :data data
                       :path (:path result)}))
      {:result (:val result)
       :vars (:vars result)})))

;;=============================================================================
;; Re-exports and utilities
;;=============================================================================

(def failure?
  "Returns true if x is a MatchFailure"
  core/failure?)

(def substitute-vars
  "Runtime substitution: walk `form` and replace symbols with values from `vars` map.
   Works with dynamic patterns at runtime.

   Example:
     (substitute-vars '(+ x x) {'x 5}) ;=> (+ 5 5)"
  core/substitute-vars)

(defn rewrite-rule
  "Create a rewrite rule that matches `from-pattern` and produces `to-pattern`.

   Returns a function that:
   - Takes data and attempts to match against from-pattern
   - On match, substitutes bound variables into to-pattern
   - Returns the rewritten form, or nil if no match

   Examples:
     (def double->add (rewrite-rule '[* 2 x] '(+ x x)))
     (double->add '(* 2 5))     ;=> (+ 5 5)
     (double->add '(+ 1 2))     ;=> nil"
  [from-pattern to-pattern]
  (let [matcher (compile from-pattern)]
    (fn [data]
      (when-let [vars (query matcher data)]
        (substitute-vars to-pattern vars)))))

(defn named-rule
  "Create a compilation rule that recognizes `name` in patterns and compiles to `pattern`.

   Examples:
     (def tokens-rule
       (named-rule 'tokens
         (list '? :-> (list '? :sub #(re-seq #\"\\S+\" %)) '[tokens*])))

     (def tokenizer (compile 'tokens [tokens-rule]))
     (query tokenizer \"hello world\")
     ;=> {tokens (\"hello\" \"world\")}"
  [name pattern]
  (let [matcher (core/ptn->matcher pattern core/core-rules)]
    (core/mchain [(core/mpred #(= name %))
                  (core/msub (constantly matcher))])))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; compile - returns matcher function
  ;;-------------------------------------------------------------------
  (def m (compile '{:a a :b b}))
  ;; callable as function - returns ValMatchResult
  (m {:a 1 :b 2}) ;=>> {:vars '{a 1 b 2}}
  ;; ignores extra keys in data
  (m {:a 1 :b 2 :c 3}) ;=>> {:vars '{a 1 b 2}}

  ;;-------------------------------------------------------------------
  ;; query - returns vars map or nil
  ;;-------------------------------------------------------------------
  (query m {:a 1 :b 2}) ;=>> '{a 1 b 2}
  (query '{:x 10} {:x 99}) ;=> nil
  (query '{:x x} {:x 42}) ;=>> '{x 42}

  ;;-------------------------------------------------------------------
  ;; match-result - returns {:vars} or MatchFailure
  ;;-------------------------------------------------------------------
  (match-result m {:a 1 :b 2}) ;=>> {:vars '{a 1 b 2}}
  (match-result '{:x 10} {:x 99}) ;=>> {:reason #"fail" :path [:x]}

  ;;-------------------------------------------------------------------
  ;; match! - returns vars or throws
  ;;-------------------------------------------------------------------
  (match! m {:a 1 :b 2}) ;=>> '{a 1 b 2}

  ;;-------------------------------------------------------------------
  ;; Top-level patterns - symbols and literals
  ;;-------------------------------------------------------------------
  (query 'x 3) ;=>> '{x 3}
  (query 3 3) ;=>> {}
  (query 3 4) ;=> nil
  (query "hello" "hello") ;=>> {}
  (query :foo :foo) ;=>> {}

  ;;-------------------------------------------------------------------
  ;; Pattern DSL - symbol form variables
  ;;-------------------------------------------------------------------
  (query '{:a a :b {:c a}} {:a 3 :b {:c 3}}) ;=>> '{a 3}
  (query '[3 _ a?] [3 0 4]) ;=>> '{a 4}

  ;;-------------------------------------------------------------------
  ;; Quantifiers: x+, x*, greedy variants
  ;;-------------------------------------------------------------------
  (query '[1 rest+] [1 2 3 4]) ;=>> '{rest [2 3 4]}
  (query '[rest+] []) ;=> nil
  (query '[1 rest*] [1 2 3]) ;=>> '{rest [2 3]}
  (query '[1 rest*] [1]) ;=>> '{rest []}
  (query '[a* b*] [1 2 3]) ;=>> '{a () b (1 2 3)}
  (query '[a*! b*] [1 2 3]) ;=>> '{a (1 2 3) b ()}
  (query '[a+ b+] [1 2 3]) ;=>> '{a (1) b (2 3)}
  (query '[a+! b+] [1 2 3]) ;=>> '{a (1 2) b (3)}

  ;;-------------------------------------------------------------------
  ;; Failure handling
  ;;-------------------------------------------------------------------
  (query '{:a 5} {:a 3}) ;=> nil
  (match-result '{:a 5} {:a 3}) ;=>> {:reason #"fail" :path [:a] :depth 1}
  (match-result '{:a {:b 10}} {:a {:b 20}}) ;=>> {:path [:a :b] :depth 2}

  ;;-------------------------------------------------------------------
  ;; List-form variables (x pred? [min max]? !?)
  ;;-------------------------------------------------------------------
  (query [(list 'x even?)] [4]) ;=>> '{x 4}
  (query [(list 'x even?)] [3]) ;=> nil
  (query '[(x #{:a :b :c})] [:b]) ;=>> '{x :b}
  (query '[(x #{:a :b :c})] [:d]) ;=> nil
  (query '[(x {:valid true :invalid false})] [:valid]) ;=>> '{x :valid}
  (query '[(x {:valid true :invalid false})] [:unknown]) ;=> nil
  (query '[(x [2 4])] [1 2 3]) ;=>> '{x (1 2 3)}
  (query '[(x [2 4])] [1]) ;=> nil
  (query [(list 'x even? [2 3])] [2 4 6]) ;=>> '{x (2 4 6)}
  (query '[(x [1 0])] [1 2 3]) ;=>> '{x (1 2 3)}
  (query '[(x [0 5]) y*] [1 2 3]) ;=>> '{x () y (1 2 3)}
  (query '[(x [0 5] !) y*] [1 2 3]) ;=>> '{x (1 2 3) y ()}

  ;;-------------------------------------------------------------------
  ;; Sets as predicates
  ;;-------------------------------------------------------------------
  (query {:status #{:active :pending}} {:status :active}) ;=>> {}
  (query {:status #{:active :pending}} {:status :inactive}) ;=> nil
  (query [#{:a :b} 'x] [:a 42]) ;=>> '{x 42}
  (query [#{:a :b} 'x] [:c 42]) ;=> nil
  (query '{:type t :status #{:ok :warn}} {:type "test" :status :ok}) ;=>> '{t "test"}

  ;;-------------------------------------------------------------------
  ;; Plain symbols and double-quoted literals
  ;;-------------------------------------------------------------------
  (query '{:a x :b y} {:a 1 :b 2}) ;=>> '{x 1 y 2}
  (query '[x y z] [1 2 3]) ;=>> '{x 1 y 2 z 3}
  (query '{:outer {:inner val}} {:outer {:inner 42}}) ;=>> '{val 42}
  (query '{:type ''foo} {:type 'foo}) ;=>> {}
  (query '{:type ''foo} {:type 'bar}) ;=> nil
  (query '[''start x ''end] ['start 42 'end]) ;=>> '{x 42}

  ;;-------------------------------------------------------------------
  ;; Sequence patterns
  ;;-------------------------------------------------------------------
  ((compile '[a b]) [1 2]) ;=>> {:vars '{a 1 b 2}}
  (query '[head tail+] [1 2 3]) ;=>> '{head 1 tail (2 3)}

  ;;-------------------------------------------------------------------
  ;; Nested patterns
  ;;-------------------------------------------------------------------
  (query '[[x]] '((inner))) ;=>> '{x inner}
  (query '[a [b c]] '(x (y z))) ;=>> '{a x b y c z}
  (query '[op [nested x y] z] '(+ (* a b) c)) ;=>> '{op + nested * x a y b z c}

  ;;-------------------------------------------------------------------
  ;; rewrite-rule
  ;;-------------------------------------------------------------------
  (def double->add (rewrite-rule '[''* 2 x] '(+ x x)))
  (double->add '(* 2 5)) ;=>> '(+ 5 5)
  (double->add '(* 2 y)) ;=>> '(+ y y)
  (double->add '(+ 1 2)) ;=> nil
  (def combine-mults (rewrite-rule '[''* 2 [''* 3 x]] '(* 6 x)))
  (combine-mults '(* 2 (* 3 y))) ;=>> '(* 6 y)
  (def swap-args (rewrite-rule '[''f a b] '(f b a)))
  (swap-args '(f 1 2)) ;=>> '(f 2 1)

  ;;-------------------------------------------------------------------
  ;; named-rule
  ;;-------------------------------------------------------------------
  (def tokens-rule
    (named-rule 'tokens
                (list '? :-> (list '? :sub #(re-seq #"\S+" %)) '[tokens*])))
  (def tokenizer (compile 'tokens [tokens-rule]))
  (query tokenizer "hello world") ;=>> '{tokens ("hello" "world")}
  (def parser (compile '{:input tokens} [tokens-rule]))
  (query parser {:input "foo bar baz"}) ;=>> '{tokens ("foo" "bar" "baz")}
  (query (compile '["parse:" tokens] [tokens-rule]) ["parse:" "a b c"])
  ;=>> '{tokens ("a" "b" "c")}

  ;;-------------------------------------------------------------------
  ;; transform
  ;;-------------------------------------------------------------------
  (transform (list '? :update inc) 5) ;=>> {:result 6 :vars {}}
  (transform (list '? :var 'n (list '? :update inc)) 5) ;=>> {:result 6 :vars {'n 6}}
  (transform (list 'n :update inc) 5) ;=>> {:result 6 :vars {'n 6}}
  (transform {:a (list 'x :update inc)} {:a 5}) ;=>> {:result {:a 6} :vars {'x 6}}
  (transform {:a {:b (list '? :update inc)}} {:a {:b 5} :c 10})
  ;=>> {:result {:a {:b 6} :c 10} :vars {}}
  (transform {:a (list '? :update inc) :b (list '? :update dec)} {:a 1 :b 10})
  ;=>> {:result {:a 2 :b 9} :vars {}}
  (transform [(list '? :update inc) 'rest*] [1 2 3])
  ;=>> {:result [2 2 3] :vars {'rest '(2 3)}}
  (transform {:x (list '? :pred even?)} {:x 3}) ;=> nil
  )
