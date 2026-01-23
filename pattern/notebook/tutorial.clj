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
;; It lets you match patterns, extract values, and transform data declaratively.
;;
;; The public API consists of:
;; - `match-fn`    - Create a function that pattern-matches and computes
;; - `failure?`    - Check if a match failed
;; - `rule`        - Create pattern → template transformation rules
;; - `apply-rules` - Apply rules recursively to transform data
;; - `compile-pattern` - Compile patterns to matcher functions (advanced)
;; - `register-var-option!` - Extend variable syntax (advanced)
;; - `register-schema-rule!` - Extend schema vocabulary (advanced)
;;
;; Key concepts:
;; - Pattern: A template describing the shape of data to match
;; - Variables: Symbols prefixed with ? (like ?x, ?name) that capture values
;; - MatchFailure: Returned when a pattern doesn't match, with diagnostics
;; - Rule: A function that transforms data matching a pattern to a template

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
;; Wildcard: ?_ matches anything but doesn't bind
;;-------------------------------------------------------------------
((p/match-fn [?_ ?middle ?_] ?middle) [1 2 3])
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
;; PART 12: Rule-Based Transformations
;;=============================================================================
;;
;; The `rule` macro creates transformation functions: pattern → template.
;; Rules are the foundation for declarative data transformation.

;;-------------------------------------------------------------------
;; Basic rule syntax
;;-------------------------------------------------------------------
;; A rule returns the template with ?vars substituted when matched,
;; or nil when the pattern doesn't match.

(def double-to-add (p/rule (* 2 ?x) (+ ?x ?x)))

(double-to-add '(* 2 5))
;=> (+ 5 5)

(double-to-add '(* 3 5))
;=> nil  ; pattern didn't match

;;-------------------------------------------------------------------
;; Rules with map patterns
;;-------------------------------------------------------------------
(def extract-point (p/rule {:x ?x :y ?y} [?x ?y]))

(extract-point {:x 10 :y 20 :z 30})
;=> [10 20]

(extract-point {:a 1 :b 2})
;=> nil

;;-------------------------------------------------------------------
;; Rules with sequence patterns
;;-------------------------------------------------------------------
(def rotate-first (p/rule [?first ?rest*] [?rest* ?first]))

(rotate-first [1 2 3 4])
;=> [[2 3 4] 1]  ; note: ?rest* is collected as vector

;;-------------------------------------------------------------------
;; apply-rules: Recursive transformation
;;-------------------------------------------------------------------
;; `apply-rules` applies rules bottom-up throughout a data structure.
;; Each node is transformed by the first matching rule.

(def simplify-mul-2 (p/rule (* 2 ?x) (+ ?x ?x)))
(def simplify-add-0 (p/rule (+ 0 ?x) ?x))

;; Single transformation
(p/apply-rules [simplify-mul-2] '(* 2 3))
;=> (+ 3 3)

;; Deep transformation
(p/apply-rules [simplify-mul-2] '(foo (* 2 5) bar))
;=> (foo (+ 5 5) bar)

;; Multiple rules, first match wins
(p/apply-rules [simplify-mul-2 simplify-add-0]
               '(+ 0 (* 2 y)))
;=> (+ y y)  ; both rules applied bottom-up

;; Nested application
(p/apply-rules [simplify-mul-2]
               '(* 2 (* 2 z)))
;=> (+ (+ z z) (+ z z))  ; applied at all levels

;;-------------------------------------------------------------------
;; Practical: Simple expression simplifier
;;-------------------------------------------------------------------
(def algebra-rules
  [(p/rule (+ 0 ?x) ?x)           ; 0 + x = x
   (p/rule (+ ?x 0) ?x)           ; x + 0 = x
   (p/rule (* 1 ?x) ?x)           ; 1 * x = x
   (p/rule (* ?x 1) ?x)           ; x * 1 = x
   (p/rule (* 0 ?x) 0)            ; 0 * x = 0
   (p/rule (* ?x 0) 0)            ; x * 0 = 0
   (p/rule (* 2 ?x) (+ ?x ?x))])  ; 2 * x = x + x

(p/apply-rules algebra-rules
               '(+ (* 1 a) (* 0 b)))
;=> (+ a 0)  ; Note: need multiple passes for full simplification

;; For full simplification, apply until fixed point:
(defn simplify [expr]
  (let [simplified (p/apply-rules algebra-rules expr)]
    (if (= simplified expr)
      expr
      (recur simplified))))

(simplify '(+ (* 1 a) (* 0 b)))
;=> a

;;=============================================================================
;; PART 13: Custom Variable Options
;;=============================================================================
;;
;; Extended variable syntax: (?var :option value ...)
;; Built-in options: :when, :default
;; You can register custom options with register-var-option!

;;-------------------------------------------------------------------
;; Built-in: :when predicate
;;-------------------------------------------------------------------
((p/match-fn {:age (?age :when pos?)} ?age) {:age 25})
;=> 25

(p/failure? ((p/match-fn {:age (?age :when pos?)} ?age) {:age -5}))
;=> true

;;-------------------------------------------------------------------
;; Built-in: :default value
;;-------------------------------------------------------------------
((p/match-fn {:name (?name :default "Anonymous")} ?name) {:name nil})
;=> "Anonymous"

((p/match-fn {:name (?name :default "Anonymous")} ?name) {:name "Alice"})
;=> "Alice"

;;-------------------------------------------------------------------
;; Combining options
;;-------------------------------------------------------------------
((p/match-fn {:count (?n :default 0 :when #(>= % 0))} ?n) {:count nil})
;=> 0

;;-------------------------------------------------------------------
;; Custom options: register-var-option!
;;-------------------------------------------------------------------
;; Register a :transform option that applies a function to the value

(p/register-var-option! :transform
                        (fn [f] (list '? :sub f)))

;; Now you can use :transform in patterns
((p/match-fn {:name (?n :transform clojure.string/upper-case)} ?n)
 {:name "alice"})
;=> "ALICE"

;; Register a :coerce option for type coercion
(p/register-var-option! :coerce
                        (fn [type-fn]
                          (list '? :-> '(? :any) (list '? :sub type-fn))))

((p/match-fn {:port (?p :coerce #(Integer/parseInt %))} ?p)
 {:port "8080"})
;=> 8080

;;=============================================================================
;; PART 14: Advanced - compile-pattern
;;=============================================================================
;;
;; For lower-level control, use compile-pattern directly.
;; It returns a matcher function that operates on internal MatchResult types.
;;
;; NOTE: This section uses internal APIs from sg.flybot.pullable.core.
;; These are useful for building custom tooling but may change between versions.

(require '[sg.flybot.pullable.core :as pc])

;; Compile a pattern to a matcher
(def name-matcher (p/compile-pattern '{:name ?n}))

;; The matcher takes a ValMatchResult (created via vmr) and returns either
;; a ValMatchResult with :val and :vars, or a MatchFailure
(name-matcher (pc/vmr {:name "Alice" :age 30}))
;=> {:val {:name "Alice" :age 30} :vars {n "Alice"}}

;; Check for failure (can use either p/failure? or pc/failure? - same fn)
(p/failure? (name-matcher (pc/vmr "not a map")))
;=> true

;;-------------------------------------------------------------------
;; Custom rewrite rules
;;-------------------------------------------------------------------
;; You can extend the pattern syntax with custom rewrite rules.

;; Define a rule for (not-nil ?x) syntax
(def not-nil-rule
  (fn [form]
    (when (and (seq? form) (= 'not-nil (first form)))
      (let [var (second form)]
        (list var :when some?)))))

;; Use with :rules option
((p/compile-pattern '(not-nil ?n) {:rules [not-nil-rule]})
 (pc/vmr "hello"))
;=> {:val "hello" :vars {n "hello"}}

((p/compile-pattern '(not-nil ?n) {:rules [not-nil-rule]})
 (pc/vmr nil))
;=> MatchFailure (predicate failed)

;; Use rules in map patterns
((p/compile-pattern '{:name (not-nil ?n)} {:rules [not-nil-rule]})
 (pc/vmr {:name "Alice"}))
;=> {:vars {n "Alice"}}

;;=============================================================================
;; PART 15: Schema Validation
;;=============================================================================
;;
;; Schemas validate pattern structure at compile time.
;; When provided, patterns that don't match the schema are rejected
;; BEFORE runtime - catching errors early.

;;-------------------------------------------------------------------
;; Basic schema types
;;-------------------------------------------------------------------
;; Type keywords: :map :seq :string :number :keyword :symbol :any

;; This pattern is valid for a map schema
(p/compile-pattern '{:name ?n} {:schema :map})
;=> <matcher-fn>

;; This would FAIL at compile time (seq pattern on map schema):
;; (p/compile-pattern '[?x] {:schema :map})
;; => ExceptionInfo: Schema violation: pattern uses :seq but schema expects :map

;;-------------------------------------------------------------------
;; Record schemas: specific fields with types
;;-------------------------------------------------------------------
(def user-schema
  {:name :string
   :age :number
   :email :string})

;; Valid: pattern only uses declared fields
(p/compile-pattern '{:name ?n :age ?a} {:schema user-schema})
;=> <matcher-fn>

;; Invalid: pattern uses undeclared field
;; (p/compile-pattern '{:name ?n :secret ?s} {:schema user-schema})
;; => ExceptionInfo: Schema violation: pattern has key :secret not defined in record schema

;;-------------------------------------------------------------------
;; Schema as Visibility Control
;;-------------------------------------------------------------------
;;
;; This is a powerful property: schemas act as "public interfaces" for data.
;; Only fields declared in the schema can be accessed by patterns.
;; Undeclared fields become effectively "private".

;; Example: Data has implementation details
(def user-data
  {:name "Alice"
   :age 30
   :_internal-id "uuid-abc123"      ; implementation detail
   :_cache {:computed-at 12345}})   ; internal state

;; Schema declares the "public API"
(def public-user-schema
  {:name :string
   :age :number})

;; Patterns can only extract public fields
(def get-user (p/compile-pattern '{:name ?n :age ?a} {:schema public-user-schema}))
(get-user (pc/vmr user-data))
;=> {:val {...} :vars {n "Alice" a 30}}

;; Attempting to access internal fields is REJECTED at compile time:
;; (p/compile-pattern '{:name ?n :_internal-id ?id} {:schema public-user-schema})
;; => ExceptionInfo: pattern has key :_internal-id not defined in record schema

;; This provides:
;; - Encapsulation: hide implementation details from consumers
;; - Versioning: schema is the contract, internals can change
;; - Security: prevent extraction of sensitive fields

;;-------------------------------------------------------------------
;; Dictionary schemas: homogeneous key/value types
;;-------------------------------------------------------------------
;; For maps where all keys share a type (not specific fields)

(p/compile-pattern '{:x ?x :y ?y} {:schema [:map-of :keyword :number]})
;=> <matcher-fn>  ; any keyword keys with number values

;;-------------------------------------------------------------------
;; Sequence schemas
;;-------------------------------------------------------------------
;; Homogeneous sequences
(p/compile-pattern '[?first ?rest*] {:schema [:number]})
;=> <matcher-fn>  ; seq of numbers

;; Tuples (fixed positions with types)
(p/compile-pattern '[?x ?y] {:schema [:tuple :number :string]})
;=> <matcher-fn>  ; [number, string] pair

;;-------------------------------------------------------------------
;; Advanced schema types
;;-------------------------------------------------------------------
;; Literal values
(p/compile-pattern '{:status ?s} {:schema {:status [:= :active]}})

;; Enums (sets)
(p/compile-pattern '{:status ?s} {:schema {:status #{:active :pending :closed}}})

;; Union types
(p/compile-pattern '{:id ?id} {:schema {:id [:or :string :number]}})

;; Optional fields
(p/compile-pattern '{:name ?n} {:schema {:name :string :bio [:optional :string]}})

;;-------------------------------------------------------------------
;; Extending schema vocabulary
;;-------------------------------------------------------------------
;; Use register-schema-rule! to add custom schema types
;; Each rule returns {:type keyword, :child-schema fn, :valid-keys set}

(p/register-schema-rule!
 (p/match-fn [:non-empty ?elem-type]
             {:type :seq
              :child-schema (constantly ?elem-type)}))

;; Now [:non-empty :number] is a valid schema for non-empty number sequences

;;=============================================================================
;; PART 16: Practical Examples
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
;; Define handlers once (they compile the pattern)
(def ping-handler (p/match-fn [ping] :pong))
(def echo-handler (p/match-fn [echo ?text] [:echoed ?text]))
(def add-handler (p/match-fn [add ?a ?b] [:sum (+ ?a ?b)]))

(defn handle-message [msg]
  ;; Try each handler, return first non-failure result
  (or (reduce (fn [_ handler]
                (let [result (handler msg)]
                  (when-not (p/failure? result)
                    (reduced result))))
              nil
              [ping-handler echo-handler add-handler])
      :unknown))

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

;;-------------------------------------------------------------------
;; Rule-based code transformation
;;-------------------------------------------------------------------
;; Transform async/await-style code to callback-style

(def async-rules
  [(p/rule (await ?expr) (fn [callback] (?expr callback)))
   (p/rule (async ?body) (fn [callback] ?body))])

(p/apply-rules async-rules
               '(async (await (fetch-data))))
;=> (fn [callback] (fn [callback] ((fetch-data) callback)))

;;-------------------------------------------------------------------
;; JSON schema-like validation
;;-------------------------------------------------------------------
(defn make-validator [schema]
  (p/match-fn schema {:valid true :data $}))

(def user-schema
  {:name string?
   :age (?age :when #(and (int? %) (pos? %)))
   :email #".*@.*"})

(def validate (make-validator user-schema))

(validate {:name "Alice" :age 30 :email "alice@example.com"})
;=> {:valid true :data {:name "Alice" :age 30 :email ["alice@example.com"]}}

;;=============================================================================
;; PART 17: Tips and Best Practices
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

;; 4. Use ?_ to ignore values you don't care about
((p/match-fn [?_ ?middle ?_] ?middle) [1 2 3])
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

;; 8. Use rules for declarative transformations
(def normalize-coords
  (p/rule {:lat ?lat :lng ?lng} {:latitude ?lat :longitude ?lng}))

(normalize-coords {:lat 40.7 :lng -74.0})
;=> {:latitude 40.7 :longitude -74.0}

;; 9. Use apply-rules for recursive transformations
(def expand-macros
  [(p/rule (when ?test ?body) (if ?test ?body nil))
   (p/rule (unless ?test ?body) (if ?test nil ?body))])

(p/apply-rules expand-macros '(when (> x 0) (print x)))
;=> (if (> x 0) (print x) nil)

;; 10. Extend variable syntax for domain-specific patterns
(p/register-var-option! :as-int
                        (fn [_] (list '? :sub #(if (string? %) (Integer/parseInt %) %))))

;; Now use :as-int to coerce string values
((p/match-fn {:port (?p :as-int _)} (+ ?p 1)) {:port "8080"})
;=> 8081

;;=============================================================================
;; Congratulations!
;;=============================================================================
;;
;; You've completed the flybot.pullable tutorial!
;;
;; Key takeaways:
;;
;; Core API:
;; - `match-fn`    - Create pattern-matching functions
;; - `failure?`    - Check if match failed
;; - `rule`        - Create pattern → template transformations
;; - `apply-rules` - Apply rules recursively (bottom-up)
;;
;; Pattern Variables:
;; - ?x      - Bind single value
;; - ?_      - Wildcard (match anything, no binding)
;; - ?x?     - Optional (0 or 1)
;; - ?x+     - One or more (lazy)
;; - ?x*     - Zero or more (lazy)
;; - ?x+!    - One or more (greedy)
;; - ?x*!    - Zero or more (greedy)
;;
;; Extended Variables:
;; - (?x :when pred)     - With predicate
;; - (?x :default val)   - With default value
;; - (?x :option val ...) - Custom options
;;
;; Special:
;; - $       - The matched/transformed value (in match-fn body)
;; - Unification - Same variable in sequence must match same value
;;
;; MatchFailure fields:
;; - :reason - Human-readable error message
;; - :path   - Location in data structure (vector of keys/indices)
;; - :value  - The value that failed to match
;;
;; Advanced:
;; - `compile-pattern` - Direct pattern compilation with custom rules
;; - `register-var-option!` - Extend variable syntax
;;
;; For more details, see PROJECT_SUMMARY.md and the source code!
