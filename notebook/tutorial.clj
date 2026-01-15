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
;; flybot.pullable is a pattern matching library for Clojure data structures.
;; It lets you match patterns and compute with the matched values.
;;
;; The public API is minimal - just two exports:
;; - `match-fn` - create a function that pattern-matches and computes
;; - `failure?` - check if a match failed
;;
;; Key concepts:
;; - Pattern: A template describing the shape of data to match
;; - Variables: Symbols prefixed with ? (like ?x, ?name) that capture values
;; - MatchFailure: Returned when a pattern doesn't match, with diagnostics

;;=============================================================================
;; PART 2: Quick Start with match-fn
;;=============================================================================
;;
;; `match-fn` creates a function that:
;; 1. Pattern-matches its argument
;; 2. Evaluates a body expression with matched variables bound
;; 3. Returns MatchFailure if the pattern doesn't match

;; Basic usage: extract and compute
((p/match-fn {:name ?name :age ?age}
             (str ?name " is " ?age " years old"))
 {:name "Alice" :age 30})
;=> "Alice is 30 years old"

;; Variables are symbols starting with ?
((p/match-fn ?x (+ ?x 10)) 5)
;=> 15

;; Match vectors
((p/match-fn [?a ?b ?c] (+ ?a ?b ?c)) [1 2 3])
;=> 6

;; When pattern doesn't match, returns MatchFailure
(p/failure? ((p/match-fn {:a ?x} ?x) "not a map"))
;=> true

;;=============================================================================
;; PART 3: The Special $ Symbol
;;=============================================================================
;;
;; Inside match-fn body, $ is bound to the matched/transformed value.
;; This is useful when you want to use the original data structure.

;; $ gives you the full matched value
((p/match-fn {:x ?x} (assoc $ :doubled (* 2 ?x)))
 {:x 5 :y 10})
;=> {:x 5 :y 10 :doubled 10}

;; Combine extraction with transformation
((p/match-fn [?first ?rest*]
             {:first ?first :rest ?rest :original $})
 [1 2 3 4])
;=> {:first 1 :rest (2 3 4) :original [1 2 3 4]}

;;=============================================================================
;; PART 4: Variable Syntax
;;=============================================================================
;;
;; Variables must start with ? prefix. Several quantifiers are available
;; for sequence matching:

;;-------------------------------------------------------------------
;; Basic: ?x captures a single value
;;-------------------------------------------------------------------
((p/match-fn [?x] ?x) [42])
;=> 42

;;-------------------------------------------------------------------
;; Wildcard: _ matches anything but doesn't bind
;;-------------------------------------------------------------------
((p/match-fn [_ ?middle _] ?middle) [1 2 3])
;=> 2

;;-------------------------------------------------------------------
;; Optional: ?x? matches 0 or 1 element
;;-------------------------------------------------------------------
((p/match-fn [?first ?second?] [?first ?second]) [1 2])
;=> [1 2]

((p/match-fn [?first ?second?] [?first ?second]) [1])
;=> [1 nil]  ; ?second? matched nothing, so ?second is nil

;;-------------------------------------------------------------------
;; One-or-more: ?x+ captures 1 or more elements (lazy)
;;-------------------------------------------------------------------
((p/match-fn [?first ?rest+] {:first ?first :rest ?rest}) [1 2 3 4])
;=> {:first 1 :rest (2 3 4)}

;; Fails if not enough elements
(p/failure? ((p/match-fn [?x+] ?x) []))
;=> true

;;-------------------------------------------------------------------
;; Zero-or-more: ?x* captures 0 or more elements (lazy)
;;-------------------------------------------------------------------
((p/match-fn [?first ?rest*] {:first ?first :rest ?rest}) [1])
;=> {:first 1 :rest ()}  ; rest* can be empty

;;-------------------------------------------------------------------
;; Unification: same variable must match same value
;;-------------------------------------------------------------------
;; When a variable appears multiple times in a sequence, all occurrences
;; must match the same value (unification).

;; Same values - succeeds
((p/match-fn [?x ?x] :equal) [5 5])
;=> :equal

;; Different values - fails with binding conflict
(p/failure? ((p/match-fn [?x ?x] :equal) [5 6]))
;=> true

;; Practical: check first and last are equal
((p/match-fn [?x ?middle* ?x] {:first ?x :middle ?middle})
 [:a :b :c :a])
;=> {:first :a :middle (:b :c)}

;; Fails when first != last
(p/failure? ((p/match-fn [?x ?middle* ?x] :ok) [:a :b :c :d]))
;=> true

;;=============================================================================
;; PART 5: Lazy vs Greedy Quantifiers
;;=============================================================================
;;
;; By default, + and * are LAZY - they match the minimum needed.
;; Add ! suffix for GREEDY matching - matches the maximum possible.

;;-------------------------------------------------------------------
;; Lazy (default): first quantifier takes minimum
;;-------------------------------------------------------------------
((p/match-fn [?a* ?b*] {:a ?a :b ?b}) [1 2 3])
;=> {:a () :b (1 2 3)}  ; a* takes 0 (minimum)

((p/match-fn [?a+ ?b+] {:a ?a :b ?b}) [1 2 3])
;=> {:a (1) :b (2 3)}   ; a+ takes 1 (minimum for +)

;;-------------------------------------------------------------------
;; Greedy (!): first quantifier takes maximum
;;-------------------------------------------------------------------
((p/match-fn [?a*! ?b*] {:a ?a :b ?b}) [1 2 3])
;=> {:a (1 2 3) :b ()}  ; a*! takes all (greedy)

((p/match-fn [?a+! ?b+] {:a ?a :b ?b}) [1 2 3])
;=> {:a (1 2) :b (3)}   ; a+! takes max while leaving 1 for b+

;;-------------------------------------------------------------------
;; Practical: head/tail vs init/last
;;-------------------------------------------------------------------
;; First element + rest (lazy default)
((p/match-fn [?head ?tail*] {:head ?head :tail ?tail}) [:a :b :c])
;=> {:head :a :tail (:b :c)}

;; All but last element (greedy)
((p/match-fn [?init*! ?last] {:init ?init :last ?last}) [:a :b :c])
;=> {:init (:a :b) :last :c}

;;=============================================================================
;; PART 6: Map Patterns
;;=============================================================================
;;
;; Map patterns match keys and extract values.

;;-------------------------------------------------------------------
;; Basic map matching
;;-------------------------------------------------------------------
((p/match-fn {:name ?name :age ?age}
             (str ?name " is " ?age))
 {:name "Alice" :age 30 :city "NYC"})
;=> "Alice is 30"  ; extra keys (:city) are ignored

;;-------------------------------------------------------------------
;; Nested maps
;;-------------------------------------------------------------------
((p/match-fn {:user {:name ?name :profile {:bio ?bio}}}
             {:name ?name :bio ?bio})
 {:user {:name "Bob" :profile {:bio "Developer"}}})
;=> {:name "Bob" :bio "Developer"}

;;-------------------------------------------------------------------
;; Combining maps and vectors
;;-------------------------------------------------------------------
((p/match-fn {:items [?first ?rest*]}
             {:first-item ?first :others ?rest})
 {:items ["apple" "banana" "cherry"]})
;=> {:first-item "apple" :others ("banana" "cherry")}

;;=============================================================================
;; PART 7: Literal Value Matching
;;=============================================================================
;;
;; Non-variable values in patterns must match exactly.

;; Exact values in vectors
((p/match-fn [1 ?x 3] ?x) [1 2 3])
;=> 2

(p/failure? ((p/match-fn [1 ?x 3] ?x) [1 2 4]))
;=> true  ; 3 != 4

;; Keywords and strings
((p/match-fn {:type :user :name ?name} ?name) {:type :user :name "Alice"})
;=> "Alice"

(p/failure? ((p/match-fn {:type :user :name ?name} ?name)
             {:type :admin :name "Bob"}))
;=> true  ; :user != :admin

;; Match literal symbols using the 'symbol form
((p/match-fn [foo ?x bar] ?x) ['foo 42 'bar])
;=> 42

;;=============================================================================
;; PART 8: Handling Match Failures
;;=============================================================================
;;
;; When a pattern doesn't match, match-fn returns a MatchFailure record.
;; Use failure? to check, then access fields for diagnostics.

;;-------------------------------------------------------------------
;; Check for failure
;;-------------------------------------------------------------------
(let [f (p/match-fn {:a ?a} ?a)
      result (f "not a map")]
  (p/failure? result))
;=> true

;;-------------------------------------------------------------------
;; Access MatchFailure fields for debugging
;;-------------------------------------------------------------------
(let [f (p/match-fn {:a ?a} ?a)
      result (f "not a map")]
  {:failed?  (p/failure? result)
   :reason   (:reason result)      ; human-readable message
   :value    (:value result)       ; the value that failed
   :path     (:path result)})      ; location in data structure
;=> {:failed? true
;    :reason "expected map, got class java.lang.String"
;    :value "not a map"
;    :path []}

;;-------------------------------------------------------------------
;; Nested failures include path
;;-------------------------------------------------------------------
(let [f (p/match-fn {:user {:name ?name}} ?name)
      result (f {:user "not a map"})]
  {:path (:path result)
   :value (:value result)})
;=> {:path [:user] :value "not a map"}

;;-------------------------------------------------------------------
;; Pattern for error handling
;;-------------------------------------------------------------------
(defn safe-extract [data]
  (let [f (p/match-fn {:x ?x :y ?y} (+ ?x ?y))
        result (f data)]
    (if (p/failure? result)
      {:error (:reason result)}
      {:result result})))

(safe-extract {:x 1 :y 2})
;=> {:result 3}

(safe-extract {:x 1})  ; missing :y
;=> {:error "..."}

;;=============================================================================
;; PART 9: Predicates and Sets
;;=============================================================================
;;
;; Functions and sets can be used as predicates in map patterns.

;; Functions as predicates (resolved automatically)
((p/match-fn {:age even?} "age is even") {:age 30})
;=> "age is even"

(p/failure? ((p/match-fn {:age even?} "ok") {:age 31}))
;=> true

;; Sets test membership
((p/match-fn {:status #{:active :pending}} "valid status")
 {:status :active})
;=> "valid status"

(p/failure? ((p/match-fn {:status #{:active :pending}} "ok")
             {:status :closed}))
;=> true

;;=============================================================================
;; PART 10: Regex Patterns
;;=============================================================================
;;
;; Regex patterns match strings and extract capture groups.

;; Basic regex match
((p/match-fn #"hello" $) "hello")
;=> ["hello"]

;; Capture groups
((p/match-fn #"(\d+)-(\d+)" $) "12-34")
;=> ["12-34" "12" "34"]

;; Regex in map patterns
((p/match-fn {:phone #"(\d{3})-(\d{4})"} $)
 {:phone "555-1234"})
;=> {:phone ["555-1234" "555" "1234"]}

;;=============================================================================
;; PART 11: Core Pattern Syntax
;;=============================================================================
;;
;; For advanced use, you can use the core (? :type ...) syntax directly.
;; This gives you access to all matcher types.

;; Explicit predicate
((p/match-fn (? :pred even?) $) 4)
;=> 4

;; Explicit value match
((p/match-fn (? :val 5) $) 5)
;=> 5

;; Variable binding with explicit syntax
((p/match-fn (? :var ?x (? :any)) ?x) 42)
;=> 42

;; Chain matchers with :->
((p/match-fn (? :-> (? :pred number?) (? :update inc)) $) 5)
;=> 6

;; Case matching
((p/match-fn (? :case ?type
                :even (? :pred even?)
                :odd (? :pred odd?))
             ?type)
 3)
;=> :odd

;;=============================================================================
;; PART 12: Practical Examples
;;=============================================================================

;;-------------------------------------------------------------------
;; Parse command-line style arguments
;;-------------------------------------------------------------------
(def parse-cmd
  (p/match-fn [?cmd ?file ?opts*]
              {:command ?cmd :file ?file :options ?opts}))

(parse-cmd ["copy" "file.txt" "--verbose" "--force"])
;=> {:command "copy" :file "file.txt" :options ("--verbose" "--force")}

;;-------------------------------------------------------------------
;; Extract from API responses
;;-------------------------------------------------------------------
(def extract-users
  (p/match-fn {:status ?status :data {:users [?first ?rest*]}}
              {:status ?status :first-user ?first :other-count (count ?rest)}))

(extract-users
 {:status 200
  :data {:users [{:name "Alice"} {:name "Bob"} {:name "Carol"}]}})
;=> {:status 200 :first-user {:name "Alice"} :other-count 2}

;;-------------------------------------------------------------------
;; Config extraction
;;-------------------------------------------------------------------
(def parse-config
  (p/match-fn {:database {:host ?host :port ?port}
               :features [?first-feature ?more*]}
              {:db-url (str "jdbc://" ?host ":" ?port)
               :features (cons ?first-feature ?more)}))

(parse-config
 {:database {:host "localhost" :port 5432}
  :features [:auth :logging :metrics]})
;=> {:db-url "jdbc://localhost:5432" :features (:auth :logging :metrics)}

;;-------------------------------------------------------------------
;; Simple dispatch based on structure
;;-------------------------------------------------------------------
(defn handle-message [msg]
  (let [ping-handler (p/match-fn [ping] :pong)
        echo-handler (p/match-fn [echo ?text] [:echoed ?text])
        add-handler (p/match-fn [add ?a ?b] [:sum (+ ?a ?b)])]
    (cond
      (not (p/failure? (ping-handler msg))) (ping-handler msg)
      (not (p/failure? (echo-handler msg))) (echo-handler msg)
      (not (p/failure? (add-handler msg))) (add-handler msg)
      :else :unknown)))

(handle-message '[ping])        ;=> :pong
(handle-message '[echo hello])  ;=> [:echoed hello]
(handle-message '[add 3 5])     ;=> [:sum 8]
(handle-message '[unknown])     ;=> :unknown

;;-------------------------------------------------------------------
;; Validation with error reporting
;;-------------------------------------------------------------------
(defn validate-user [user]
  (let [validator (p/match-fn {:name ?n :email ?e :age ?a}
                              {:valid true :name ?n :email ?e :age ?a})
        result (validator user)]
    (if (p/failure? result)
      {:valid false
       :error (:reason result)
       :path (:path result)}
      result)))

(validate-user {:name "Alice" :email "alice@example.com" :age 30})
;=> {:valid true :name "Alice" :email "alice@example.com" :age 30}

(validate-user {:name "Bob"})  ; missing keys
;=> {:valid false :error "..." :path [...]}

;;=============================================================================
;; PART 13: Tips and Best Practices
;;=============================================================================

;; 1. match-fn compiles the pattern once - store and reuse for performance
(def user-extractor
  (p/match-fn {:name ?name :age ?age} {:name ?name :age ?age}))

;; 2. Use lazy quantifiers (default) for head/tail patterns
((p/match-fn [?head ?tail*] {:head ?head :tail ?tail}) [1 2 3])
;=> {:head 1 :tail (2 3)}

;; 3. Use greedy quantifiers (!) for init/last patterns
((p/match-fn [?init*! ?last] {:init ?init :last ?last}) [1 2 3])
;=> {:init (1 2) :last 3}

;; 4. Use _ to ignore values you don't care about
((p/match-fn [_ ?middle _] ?middle) [1 2 3])
;=> 2

;; 5. Access MatchFailure fields for debugging
(let [result ((p/match-fn {:x ?x} ?x) "bad")]
  (when (p/failure? result)
    (println "Failed at" (:path result) ":" (:reason result))))

;; 6. Use $ when you need the original matched value
((p/match-fn {:id ?id} (assoc $ :processed true)) {:id 123 :data "..."})
;=> {:id 123 :data "..." :processed true}

;; 7. Predicates work inline in map patterns
((p/match-fn {:age pos? :status #{:active :pending}} "valid")
 {:age 25 :status :active})
;=> "valid"

;;=============================================================================
;; Congratulations!
;;=============================================================================
;;
;; You've completed the flybot.pullable tutorial!
;;
;; Key takeaways:
;;
;; API:
;; - `match-fn` - create pattern-matching functions
;; - `failure?` - check if match failed
;;
;; Pattern Variables:
;; - ?x      - bind single value
;; - _       - wildcard (match anything, no binding)
;; - ?x?     - optional (0 or 1)
;; - ?x+     - one or more (lazy)
;; - ?x*     - zero or more (lazy)
;; - ?x+!    - one or more (greedy)
;; - ?x*!    - zero or more (greedy)
;;
;; Special:
;; - $       - the matched/transformed value (in match-fn body)
;; - Unification - same variable in sequence must match same value
;;
;; MatchFailure fields:
;; - :reason - human-readable error message
;; - :path   - location in data structure (vector of keys/indices)
;; - :value  - the value that failed to match
;;
;; For more details, see PROJECT_SUMMARY.md and the source code!
