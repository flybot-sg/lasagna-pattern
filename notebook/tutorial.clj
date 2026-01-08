(ns tutorial
  "A step-by-step tutorial for flybot.pullable pattern matching library.

   Work through this file from top to bottom, evaluating forms in your REPL.
   Each section builds on the previous one.

   Start your REPL with: bb dev"
  (:require [sg.flybot.pullable :as p]))

;;=============================================================================
;; PART 1: Introduction
;;=============================================================================
;;
;; flybot.pullable is a declarative pattern matching and data extraction library.
;; Think of it as a query language for Clojure data structures.
;;
;; Key concepts:
;; - Pattern: A template that describes the shape of data you want to match
;; - Variables: Placeholders (like ?x, ?name) that capture values from the data
;; - Matcher: A compiled pattern that can be applied to data
;;
;; The library is inspired by Datomic Pull and GraphQL, providing a small,
;; composable core for data transformation.

;;=============================================================================
;; PART 2: Quick Start
;;=============================================================================

;; The main entry point is `p/query` - give it a pattern and data:

(comment
  ;; Match a map and extract values
  (p/query '{:name ?name :age ?age}
           {:name "Alice" :age 30})
  ;=> {name "Alice", age 30}

  ;; Variables are symbols starting with ?
  ;; The result is a map of variable bindings (symbol -> value)

  ;; Match a vector and extract elements
  (p/query '[?first ?second ?third]
           [1 2 3])
  ;=> {first 1, second 2, third 3}

  ;; When the pattern doesn't match, returns nil
  (p/query '[?x ?y] [1 2 3])  ; pattern expects 2 elements, data has 3
  ;=> nil
  )

;;=============================================================================
;; PART 3: Compiling Patterns
;;=============================================================================
;;
;; For performance, compile patterns once and reuse them.
;; p/query with a raw pattern compiles on every call - fine for REPL,
;; but inefficient in production code.

(comment
  ;; Compile a pattern once
  (def person-pattern (p/compile '{:name ?name :age ?age}))

  ;; Use it many times - much faster than raw patterns
  (p/query person-pattern {:name "Alice" :age 30})
  ;=> {name "Alice", age 30}

  (p/query person-pattern {:name "Bob" :age 25})
  ;=> {name "Bob", age 25}

  ;; Compiled matchers are also callable as functions
  ;; This returns the full match result (useful for debugging)
  (person-pattern {:name "Alice" :age 30})
  ;=> #ValMatchResult{:val {:name "Alice", :age 30}, :vars {name "Alice", age 30}}
  )

;;=============================================================================
;; PART 4: Result Handling
;;=============================================================================
;;
;; There are multiple ways to get results from a pattern match:

(comment
  (def m (p/compile '{:x ?x}))

  ;; 1. query - returns vars map or nil (most common)
  (p/query m {:x 42})  ;=> {x 42}
  (p/query m {:y 42})  ;=> {x nil}  ; missing key binds to nil

  ;; 2. match-result - returns {:vars ...} or MatchFailure with diagnostics
  (p/match-result m {:x 42})  ;=> {:vars {x 42}}

  ;; For value mismatches, you get a MatchFailure:
  (p/match-result '{:x 5} {:x 3})
  ;=> MatchFailure with :reason, :path, :value for debugging

  ;; Check if a result is a failure
  (p/failure? (p/match-result '{:x 5} {:x 3}))  ;=> true

  ;; 3. match! - returns vars or throws (useful in pipelines)
  (p/match! m {:x 42})  ;=> {x 42}
  ;; (p/match! '{:x 5} {:x 3})  ; throws ExceptionInfo
  )

;;=============================================================================
;; PART 5: Variable Binding Syntax
;;=============================================================================
;;
;; Variables use the ?name syntax. Several modifiers are available:

(comment
  ;;-------------------------------------------------------------------
  ;; Basic binding: ?x captures a single value
  ;;-------------------------------------------------------------------
  (p/query '[?x] [42])
  ;=> {x 42}

  ;;-------------------------------------------------------------------
  ;; Wildcard: ?_ matches anything but doesn't bind
  ;;-------------------------------------------------------------------
  (p/query '[?_ ?x ?_] [1 2 3])
  ;=> {x 2}  ; only x is bound

  ;;-------------------------------------------------------------------
  ;; Optional: ?x? matches 0 or 1 element
  ;;-------------------------------------------------------------------
  (p/query '[?x ?y?] [1 2])
  ;=> {x 1, y 2}

  (p/query '[?x ?y?] [1])  ; y? matches nothing (optional)
  ;=> {x 1}

  ;;-------------------------------------------------------------------
  ;; One-or-more: ?x+ captures remaining elements (at least 1)
  ;;-------------------------------------------------------------------
  (p/query '[?first ?rest+] [1 2 3 4])
  ;=> {first 1, rest (2 3 4)}

  (p/query '[?rest+] [])  ; fails - needs at least 1 element
  ;=> nil

  ;;-------------------------------------------------------------------
  ;; Zero-or-more: ?x* captures remaining elements (0 or more)
  ;;-------------------------------------------------------------------
  (p/query '[?first ?rest*] [1 2 3])
  ;=> {first 1, rest (2 3)}

  (p/query '[?first ?rest*] [1])  ; rest* can be empty
  ;=> {first 1, rest ()}
  )

;;=============================================================================
;; PART 6: Lazy vs Greedy Quantifiers
;;=============================================================================
;;
;; By default, + and * are LAZY - they match the minimum needed.
;; Add ! for GREEDY matching - matches the maximum possible.

(comment
  ;;-------------------------------------------------------------------
  ;; Lazy (default): first quantifier takes minimum
  ;;-------------------------------------------------------------------
  (p/query '[?a* ?b*] [1 2 3])
  ;=> {a (), b (1 2 3)}  ; a* takes 0 (minimum)

  (p/query '[?a+ ?b+] [1 2 3])
  ;=> {a (1), b (2 3)}   ; a+ takes 1 (minimum for +)

  ;;-------------------------------------------------------------------
  ;; Greedy (!): first quantifier takes maximum
  ;;-------------------------------------------------------------------
  (p/query '[?a*! ?b*] [1 2 3])
  ;=> {a (1 2 3), b ()}  ; a*! takes all (maximum)

  (p/query '[?a+! ?b+] [1 2 3])
  ;=> {a (1 2), b (3)}   ; a+! takes max while leaving 1 for b+

  ;;-------------------------------------------------------------------
  ;; Use this for head/tail patterns
  ;;-------------------------------------------------------------------
  ;; First element + rest (lazy)
  (p/query '[?head ?tail*] [:a :b :c])
  ;=> {head :a, tail (:b :c)}

  ;; All but last element (greedy)
  (p/query '[?init*! ?last] [:a :b :c])
  ;=> {init (:a :b), last :c}
  )

;;=============================================================================
;; PART 7: List-form Variables
;;=============================================================================
;;
;; For more control, use the list form: (?x pred? [min max]? !?)
;; This allows predicates and exact length control.

(comment
  ;;-------------------------------------------------------------------
  ;; With predicate: (?x pred) - value must satisfy predicate
  ;;-------------------------------------------------------------------
  (p/query '[(?x even?)] [4])
  ;=> {x 4}

  (p/query '[(?x even?)] [3])  ; fails - 3 is not even
  ;=> nil

  ;;-------------------------------------------------------------------
  ;; With length: (?x [min max]) - subsequence of min to max elements
  ;;-------------------------------------------------------------------
  (p/query '[(?x [2 4])] [1 2 3])  ; 2-4 elements
  ;=> {x (1 2 3)}

  (p/query '[(?x [2 4])] [1])  ; fails - only 1 element
  ;=> nil

  ;; [min 0] means unbounded (match to end of sequence)
  (p/query '[(?x [1 0])] [1 2 3 4 5])
  ;=> {x (1 2 3 4 5)}

  ;;-------------------------------------------------------------------
  ;; Predicate + length: each element must satisfy predicate
  ;;-------------------------------------------------------------------
  (p/query '[(?evens even? [2 3])] [2 4 6])
  ;=> {evens (2 4 6)}

  (p/query '[(?evens even? [2 3])] [2 4 5])  ; 5 is not even
  ;=> nil

  ;;-------------------------------------------------------------------
  ;; Greedy with list form: add ! at end
  ;;-------------------------------------------------------------------
  (p/query '[(?x [0 5]) ?y*] [1 2 3])   ; lazy
  ;=> {x (), y (1 2 3)}

  (p/query '[(?x [0 5] !) ?y*] [1 2 3]) ; greedy
  ;=> {x (1 2 3), y ()}
  )

;;=============================================================================
;; PART 8: Map Patterns
;;=============================================================================
;;
;; Map patterns match structural shapes.
;; Note: Missing keys bind to nil rather than failing the match.

(comment
  ;;-------------------------------------------------------------------
  ;; Basic map matching
  ;;-------------------------------------------------------------------
  (p/query '{:name ?name :age ?age}
           {:name "Alice" :age 30 :city "NYC"})
  ;=> {name "Alice", age 30}  ; extra keys ignored

  ;; Missing key binds to nil
  (p/query '{:name ?name :age ?age}
           {:name "Alice"})
  ;=> {name "Alice", age nil}

  ;;-------------------------------------------------------------------
  ;; Nested maps
  ;;-------------------------------------------------------------------
  (p/query '{:user {:name ?name :profile {:bio ?bio}}}
           {:user {:name "Bob" :profile {:bio "Developer"}}})
  ;=> {name "Bob", bio "Developer"}

  ;;-------------------------------------------------------------------
  ;; Combining maps and vectors
  ;;-------------------------------------------------------------------
  (p/query '{:items [?first ?rest*]}
           {:items ["apple" "banana" "cherry"]})
  ;=> {first "apple", rest ("banana" "cherry")}

  ;;-------------------------------------------------------------------
  ;; Nested map/vector combinations
  ;;-------------------------------------------------------------------
  (p/query '{:users [{:name ?name1} {:name ?name2}]}
           {:users [{:name "Alice"} {:name "Bob"}]})
  ;=> {name1 "Alice", name2 "Bob"}
  )

;;=============================================================================
;; PART 9: Variable Unification
;;=============================================================================
;;
;; When the same variable appears multiple times in a sequence pattern,
;; all occurrences must match the same value.

(comment
  ;;-------------------------------------------------------------------
  ;; Sequence unification - same variable must match same value
  ;;-------------------------------------------------------------------
  (p/query '[?x ?x] [1 1])  ; both 1 - matches
  ;=> {x 1}

  (p/query '[?x ?x] [1 2])  ; 1 != 2 - fails
  ;=> nil

  ;; Useful for finding duplicates or equality checks
  (p/query '[?a ?b ?a] [1 2 1])  ; first and third must match
  ;=> {a 1, b 2}

  (p/query '[?a ?b ?a] [1 2 3])  ; 1 != 3 - fails
  ;=> nil
  )

;;=============================================================================
;; PART 10: Value Matching with Literals
;;=============================================================================
;;
;; Literal values in patterns must match exactly.

(comment
  ;; Exact value in sequence
  (p/query '[1 ?x 3] [1 2 3])
  ;=> {x 2}

  (p/query '[1 ?x 3] [1 2 4])  ; 3 != 4
  ;=> nil

  ;; Exact value in map
  (p/query '{:type "user" :name ?name} {:type "user" :name "Alice"})
  ;=> {name "Alice"}

  (p/query '{:type "user" :name ?name} {:type "admin" :name "Bob"})
  ;=> nil  ; "user" != "admin"

  ;; Keywords, numbers, strings all work
  (p/query '[?x :separator ?y] [1 :separator 2])
  ;=> {x 1, y 2}
  )

;;=============================================================================
;; PART 11: Term Rewriting
;;=============================================================================
;;
;; Use patterns to transform data structures.

(comment
  ;;-------------------------------------------------------------------
  ;; rewrite-rule - create pattern-based transformations
  ;;-------------------------------------------------------------------
  ;; Transform (* 2 x) into (+ x x)
  (def double->add (p/rewrite-rule '[* 2 ?x] '(+ ?x ?x)))

  (double->add '(* 2 5))
  ;=> (+ 5 5)

  (double->add '(* 2 y))
  ;=> (+ y y)

  (double->add '(+ 1 2))  ; doesn't match
  ;=> nil

  ;;-------------------------------------------------------------------
  ;; More complex rewrites
  ;;-------------------------------------------------------------------
  ;; Combine nested multiplications
  (def combine-mults (p/rewrite-rule '[* 2 [* 3 ?x]] '(* 6 ?x)))

  (combine-mults '(* 2 (* 3 y)))
  ;=> (* 6 y)

  ;; Swap arguments
  (def swap-args (p/rewrite-rule '[f ?a ?b] '(f ?b ?a)))

  (swap-args '(f 1 2))
  ;=> (f 2 1)

  ;;-------------------------------------------------------------------
  ;; substitute-vars - runtime variable substitution
  ;;-------------------------------------------------------------------
  ;; Useful for building forms dynamically
  (p/substitute-vars '(+ ?x ?y) '{x 3 y 5})
  ;=> (+ 3 5)

  (p/substitute-vars '{:name ?name :age ?age} '{name "Alice" age 30})
  ;=> {:name "Alice", :age 30}
  )

;;=============================================================================
;; PART 12: Error Handling and Debugging
;;=============================================================================
;;
;; When patterns don't match, get detailed diagnostics.

(comment
  ;;-------------------------------------------------------------------
  ;; match-result gives failure details
  ;;-------------------------------------------------------------------
  (def result (p/match-result '{:a 5} {:a 3}))

  (p/failure? result)  ;=> true

  ;; MatchFailure contains:
  ;; - :reason - what went wrong
  ;; - :path - where in the data structure
  ;; - :value - the actual value that failed

  (:reason result)  ;=> "value mismatch: expected 5, got 3"
  (:path result)    ;=> [:a]
  (:value result)   ;=> 3

  ;;-------------------------------------------------------------------
  ;; Nested failures track full path
  ;;-------------------------------------------------------------------
  (def nested-result
    (p/match-result '{:user {:profile {:age 30}}}
                    {:user {:profile {:age 25}}}))

  (:path nested-result)  ;=> [:user :profile :age]
  (:reason nested-result) ;=> "value mismatch: expected 30, got 25"

  ;;-------------------------------------------------------------------
  ;; match! throws with details
  ;;-------------------------------------------------------------------
  ;; Use in pipelines where failure should be exceptional
  (try
    (p/match! '{:required 5} {:required 1})
    (catch Exception e
      (ex-data e)))
  ;; Returns map with :failure, :pattern, :data, :path
  )

;;=============================================================================
;; PART 13: Practical Examples
;;=============================================================================

(comment
  ;;-------------------------------------------------------------------
  ;; Parsing command-line args
  ;;-------------------------------------------------------------------
  (defn parse-args [args]
    (p/query '[?cmd ?file ?opts*] args))

  (parse-args ["copy" "file.txt" "--verbose" "--force"])
  ;=> {cmd "copy", file "file.txt", opts ("--verbose" "--force")}

  ;;-------------------------------------------------------------------
  ;; Destructuring API responses
  ;;-------------------------------------------------------------------
  (def api-pattern
    (p/compile '{:status ?status
                 :data {:users [?first-user ?rest*]}}))

  (p/query api-pattern
           {:status 200
            :data {:users [{:name "Alice"} {:name "Bob"}]}})
  ;=> {status 200, first-user {:name "Alice"}, rest ({:name "Bob"})}

  ;;-------------------------------------------------------------------
  ;; Extracting nested data
  ;;-------------------------------------------------------------------
  (def config-pattern
    (p/compile '{:database {:host ?host :port ?port}
                 :features [?first-feature ?more*]}))

  (p/query config-pattern
           {:database {:host "localhost" :port 5432}
            :features [:auth :logging :metrics]})
  ;=> {host "localhost", port 5432, first-feature :auth, more (:logging :metrics)}

  ;;-------------------------------------------------------------------
  ;; Pattern-based dispatch
  ;;-------------------------------------------------------------------
  (defn handle-message [msg]
    (cond
      (p/query '[ping] msg)
      :pong

      (p/query '[echo ?text] msg)
      (let [{:syms [text]} (p/query '[echo ?text] msg)]
        [:echoed text])

      (p/query '[add ?a ?b] msg)
      (let [{:syms [a b]} (p/query '[add ?a ?b] msg)]
        [:sum (+ a b)])

      :else
      :unknown))

  (handle-message '[ping])        ;=> :pong
  (handle-message '[echo hello])  ;=> [:echoed hello]
  (handle-message '[add 3 5])     ;=> [:sum 8]

  ;;-------------------------------------------------------------------
  ;; AST transformation with rewrite rules
  ;;-------------------------------------------------------------------
  ;; Simple constant folding
  (defn fold-add [expr]
    (when-let [vars (p/query '[+ ?a ?b] expr)]
      (let [{:syms [a b]} vars]
        (when (and (number? a) (number? b))
          (+ a b)))))

  (fold-add '(+ 2 3))   ;=> 5
  (fold-add '(+ x 3))   ;=> nil (x is not a number)
  (fold-add '(* 2 3))   ;=> nil (not an add expression)
  )

;;=============================================================================
;; PART 14: Tips and Best Practices
;;=============================================================================

(comment
  ;; 1. Compile patterns that are used repeatedly
  ;; BAD:  (map #(p/query '{:x ?x} %) data)  ; compiles N times
  ;; GOOD: (let [p (p/compile '{:x ?x})] (map #(p/query p %) data))

  ;; 2. Use match-result for validation with error messages
  (defn validate-user [user]
    (let [result (p/match-result '{:name ?name :age ?age} user)]
      (if (p/failure? result)
        {:error (:reason result) :path (:path result)}
        {:valid true :data (:vars result)})))

  ;; 3. Use lazy quantifiers (default) for head/tail patterns
  (p/query '[?head ?tail*] [1 2 3])  ; head gets first, tail gets rest

  ;; 4. Use greedy quantifiers for init/last patterns
  (p/query '[?init*! ?last] [1 2 3])  ; init gets all but last

  ;; 5. Sequence variable unification catches inconsistencies
  (p/query '[?x ?y ?x] [1 2 1])  ;=> {x 1, y 2}
  (p/query '[?x ?y ?x] [1 2 3])  ;=> nil (1 != 3)

  ;; 6. Use ?_ to ignore values you don't need
  (p/query '[?_ ?middle ?_] [1 2 3])  ;=> {middle 2}

  ;; 7. Use rewrite-rule for simple transformations
  (def normalize-let
    (p/rewrite-rule '[let [?binding ?value] ?body]
                    '(let* [?binding ?value] ?body))))

;;=============================================================================
;; Congratulations!
;;=============================================================================
;;
;; You've completed the flybot.pullable tutorial!
;;
;; Key takeaways:
;; - Use p/query for extracting variable bindings
;; - Use p/compile for performance-critical code
;; - Variable syntax: ?x, ?x?, ?x+, ?x*, ?x+!, ?x*!
;; - Map and vector patterns match structurally
;; - List-form (?x pred? [min max]? !?) for fine control
;; - rewrite-rule for pattern-based transformations
;; - match-result for detailed error diagnostics
;;
;; For more details, see PROJECT_SUMMARY.md and the source code!
