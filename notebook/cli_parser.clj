(ns cli-parser
  "A comprehensive CLI argument parser demonstrating flybot.pullable patterns.

   This demo builds a Unix-style command line argument parser that handles:
   - Short options with values: -f file.txt
   - Combined short flags: -abc expands to -a -b -c
   - Long options (switches): --verbose
   - Long options with values: --output=file.txt or --output file.txt
   - End of options marker: -- (everything after is positional)
   - Positional arguments: file1.txt file2.txt

   Work through this file from top to bottom, evaluating forms in your REPL.
   Start your REPL with: bb dev"
  (:require [sg.flybot.pullable :as p]))

;;=============================================================================
;; PART 1: The Problem
;;=============================================================================
;;
;; Unix command-line argument parsing follows specific conventions:
;;
;; | Syntax | Meaning | Example |
;; |--------|---------|---------|
;; | -x val | Short option with value | -f input.txt |
;; | -abc | Combined short flags | -vd = -v -d |
;; | --name | Long flag (boolean) | --verbose |
;; | --name=val | Long option with value | --output=out.txt |
;; | --name val | Long option with value | --output out.txt |
;; | -- | End options marker | -- -looks-like-flag |
;; | other | Positional argument | file.txt |
;;
;; We'll use pattern matching to:
;; 1. Classify each token by type
;; 2. Expand combined short options
;; 3. Pair options with their values
;; 4. Build a structured result

;;=============================================================================
;; PART 2: Token Classification with Regex
;;=============================================================================
;;
;; First, let's classify individual tokens using regex patterns.
;; We use Clojure's native regex for simple token classification,
;; reserving the pattern library for more complex sequence matching.

;; Regex patterns for token classification
(def ^:private long-opt-with-value-re
  "Pattern for --name=value"
  #"--([a-zA-Z][-a-zA-Z0-9]*)=(.+)")

(def ^:private long-flag-re
  "Pattern for --name (long flag/option without embedded value)"
  #"--([a-zA-Z][-a-zA-Z0-9]*)")

(def ^:private short-opts-re
  "Pattern for -abc (short options, possibly combined)"
  #"-([a-zA-Z]+)")

(comment
  ;; Test regex patterns directly
  (re-matches long-opt-with-value-re "--output=file.txt")
  ;=> ["--output=file.txt" "output" "file.txt"]

  (re-matches long-flag-re "--verbose")
  ;=> ["--verbose" "verbose"]

  (re-matches short-opts-re "-vdf")
  ;=> ["-vdf" "vdf"]

  (re-matches long-flag-re "positional")
  ;=> nil
  )

;;=============================================================================
;; PART 3: Token Classifier using :match-case
;;=============================================================================
;;
;; Use :match-case to try multiple patterns in order and identify which matched.
;; This is more declarative than nested or/when-let.

;; Build the classifier using :or pattern (public API only)
;; Each branch: (? :-> regex/matcher (? :sub transform-fn))
;; The :or tries each in order, returns first successful match
(def ^:private token-classifier
  "Compiled matcher that classifies a token string into its type.
   Uses :or to try patterns in order (most specific first)."
  (p/compile
   (list '? :or
         ;; --name=value (most specific, must come first)
         (list '? :-> long-opt-with-value-re
               (list '? :sub (fn [[_ name value]]
                               {:type :long-opt :name name :value value})))
         ;; --name (long flag)
         (list '? :-> long-flag-re
               (list '? :sub (fn [[_ name]]
                               {:type :long-flag :name name})))
         ;; -abc (short options)
         (list '? :-> short-opts-re
               (list '? :sub (fn [[_ chars]]
                               {:type :short-opts :chars chars})))
         ;; -- (end of options)
         (list '? :-> '(? :val "--")
               (list '? :sub (constantly {:type :end-opts})))
         ;; Everything else is positional
         (list '? :sub (fn [s] {:type :positional :value s})))))

(defn classify-token
  "Classify a single CLI token into its type.
   Order matters: more specific patterns (--name=val) before general (--name).

   Returns a map with :type and relevant fields."
  [token]
  (:val (token-classifier token)))

(comment
  ;; Test the combined classifier
  (classify-token "--output=file.txt")
  ;=> {:type :long-opt, :name "output", :value "file.txt"}

  (classify-token "--verbose")
  ;=> {:type :long-flag, :name "verbose"}

  (classify-token "-vdf")
  ;=> {:type :short-opts, :chars "vdf"}

  (classify-token "--")
  ;=> {:type :end-opts}

  (classify-token "input.txt")
  ;=> {:type :positional, :value "input.txt"}

  ;; Classify a sequence of tokens
  (mapv classify-token ["-v" "--output=out.txt" "file.txt"])
  ;=> [{:type :short-opts, :chars "v"}
  ;    {:type :long-opt, :name "output", :value "out.txt"}
  ;    {:type :positional, :value "file.txt"}]
  )

;;=============================================================================
;; PART 4: Expanding Combined Short Options
;;=============================================================================
;;
;; Unix allows -abc as shorthand for -a -b -c.
;; We'll use pattern matching to detect and expand these.

(defn expand-short-opts
  "Expand combined short options: -abc becomes [{:type :short-opt :char \\a} ...]"
  [token]
  (if (= :short-opts (:type token))
    (mapv (fn [c] {:type :short-opt :char c}) (:chars token))
    [token]))

(comment
  ;; Single character stays as one token
  (expand-short-opts {:type :short-opts :chars "v"})
  ;=> [{:type :short-opt, :char \v}]

  ;; Multiple characters expand into multiple tokens
  (expand-short-opts {:type :short-opts :chars "vdf"})
  ;=> [{:type :short-opt, :char \v}
  ;    {:type :short-opt, :char \d}
  ;    {:type :short-opt, :char \f}]

  ;; Other token types pass through unchanged
  (expand-short-opts {:type :long-flag :name "verbose"})
  ;=> [{:type :long-flag, :name "verbose"}]
  )

;;=============================================================================
;; PART 5: Schema Definition
;;=============================================================================
;;
;; Define which options expect values vs are boolean flags.

(def example-schema
  "Example CLI schema for a file processing tool."
  {:short-opts
   {\f :file      ;; -f expects a value, maps to :file
    \o :output}   ;; -o expects a value, maps to :output

   :long-opts
   {"file"   :file     ;; --file expects a value
    "output" :output   ;; --output expects a value
    "config" :config}  ;; --config expects a value

   :flags
   #{\v \d \h                            ;; -v, -d, -h are flags
     "verbose" "debug" "help" "dry-run"} ;; long forms
   })

(defn flag?
  "Check if an option is a flag (no value expected)."
  [schema token]
  (case (:type token)
    :short-opt (contains? (:flags schema) (:char token))
    :long-flag (contains? (:flags schema) (:name token))
    false))

(defn option-key
  "Get the canonical key for an option."
  [schema token]
  (case (:type token)
    :short-opt (get-in schema [:short-opts (:char token)])
    :long-flag (get-in schema [:long-opts (:name token)])
    :long-opt  (get-in schema [:long-opts (:name token)])
    nil))

(comment
  ;; Check if tokens are flags
  (flag? example-schema {:type :short-opt :char \v})
  ;=> true

  (flag? example-schema {:type :short-opt :char \f})
  ;=> false (it expects a value)

  (flag? example-schema {:type :long-flag :name "verbose"})
  ;=> true

  ;; Get canonical option keys
  (option-key example-schema {:type :short-opt :char \f})
  ;=> :file

  (option-key example-schema {:type :long-opt :name "output" :value "x"})
  ;=> :output
  )

;;=============================================================================
;; PART 6: Pattern-Based Token Stream Parsing
;;=============================================================================
;;
;; This is where pattern matching really shines! We define patterns for
;; different token sequences and use them to consume the token stream.
;;
;; Using plain symbols for variables makes patterns more readable:
;;   [end ?rest*]  instead of  [?end ?rest*]

;;-----------------------------------------------------------------------------
;; Pattern factory - creates schema-aware patterns
;;-----------------------------------------------------------------------------
;; We create patterns as a function of schema so flag/option distinction
;; is baked into the predicates.

(defn make-patterns
  "Create token stream patterns with schema-aware predicates.

   Returns a vector of [case-key pattern] pairs in priority order:
   - :end-opts   - The -- marker
   - :long-opt   - --name=value (has embedded value)
   - :opt-value  - Non-flag option followed by value
   - :flag       - Boolean flag (no value)
   - :opt-missing - Option needs value but none follows
   - :positional - Regular positional argument

   NOTE: Map/set/keyword predicates in the library work as key-lookup functions,
   not as submap matchers. For token type matching we need anonymous functions."
  [schema]
  [[:end-opts
    (p/compile [(list 'end #(= :end-opts (:type %))) '?rest*])]

   [:long-opt
    (p/compile [(list 'opt #(= :long-opt (:type %))) '?rest*])]

   [:opt-value
    (p/compile [(list 'opt #(and (#{:short-opt :long-flag} (:type %))
                                 (not (flag? schema %))))
                (list 'val #(= :positional (:type %)))
                '?rest*])]

   [:flag
    (p/compile [(list 'opt #(and (#{:short-opt :long-flag} (:type %))
                                 (flag? schema %)))
                '?rest*])]

   [:opt-missing
    (p/compile [(list 'opt #(and (#{:short-opt :long-flag} (:type %))
                                 (not (flag? schema %))))
                '?rest*])]

   [:positional
    (p/compile [(list 'pos #(= :positional (:type %))) '?rest*])]])

(defn match-first
  "Try patterns in order, return [case-key match] for first match, or nil."
  [patterns tokens]
  (some (fn [[case-key pattern]]
          (when-let [match (p/query pattern tokens)]
            [case-key match]))
        patterns))

(comment
  ;; Test the patterns
  (def test-patterns (make-patterns example-schema))

  ;; Matches :end-opts case
  (match-first test-patterns [{:type :end-opts} {:type :positional :value "x"}])
  ;=> [:end-opts {end {:type :end-opts}, rest ({:type :positional, :value "x"})}]

  ;; Matches :long-opt case
  (match-first test-patterns [{:type :long-opt :name "output" :value "file.txt"}])
  ;=> [:long-opt {opt {:type :long-opt, :name "output", :value "file.txt"}, rest ()}]

  ;; Matches :opt-value case (-f expects value, so matches opt+val)
  (match-first test-patterns [{:type :short-opt :char \f}
                              {:type :positional :value "file.txt"}])
  ;=> [:opt-value {opt {:type :short-opt, :char \f},
  ;                val {:type :positional, :value "file.txt"}, rest ()}]

  ;; Matches :flag case (-v is a flag)
  (match-first test-patterns [{:type :short-opt :char \v}
                              {:type :positional :value "file.txt"}])
  ;=> [:flag {opt {:type :short-opt, :char \v},
  ;           rest ({:type :positional, :value "file.txt"})}]
  )

;;-----------------------------------------------------------------------------
;; Pattern handlers - each returns [updated-result remaining-tokens]
;;-----------------------------------------------------------------------------

(defn- reconstruct-token
  "Reconstruct original string from classified token"
  [token]
  (case (:type token)
    :positional (:value token)
    :short-opt (str "-" (:char token))
    :long-flag (str "--" (:name token))
    :long-opt (str "--" (:name token) "=" (:value token))
    :short-opts (str "-" (:chars token))
    (str token)))

(defn handle-end-opts
  "Handle end-of-options: collect all remaining as positional"
  [{:syms [rest]} result _schema]
  (let [values (map reconstruct-token rest)]
    [(update result :positional into values) []]))

(defn handle-long-opt
  "Handle --name=value"
  [{:syms [opt rest]} result schema]
  (if-let [k (option-key schema opt)]
    [(assoc-in result [:options k] (:value opt)) rest]
    [(update result :errors conj {:error :unknown-option :token opt}) rest]))

(defn handle-opt-value
  "Handle option followed by value"
  [{:syms [opt val rest]} result schema]
  (if-let [k (option-key schema opt)]
    [(assoc-in result [:options k] (:value val)) rest]
    [(update result :errors conj {:error :unknown-option :token opt}) rest]))

(defn handle-flag
  "Handle a flag (boolean option)"
  [{:syms [opt rest]} result _schema]
  (let [flag-name (or (:name opt) (str (:char opt)))]
    [(update result :flags conj (keyword flag-name)) rest]))

(defn handle-opt-missing-value
  "Handle option that needs a value but doesn't have one"
  [{:syms [opt rest]} result _schema]
  [(update result :errors conj {:error :missing-value :option opt}) rest])

(defn handle-positional
  "Handle positional argument"
  [{:syms [pos rest]} result _schema]
  [(update result :positional conj (:value pos)) rest])

;;-----------------------------------------------------------------------------
;; Pattern-based token processor
;;-----------------------------------------------------------------------------

(def ^:private handlers
  "Map of case keys to handler functions."
  {:end-opts    handle-end-opts
   :long-opt    handle-long-opt
   :opt-value   handle-opt-value
   :flag        handle-flag
   :opt-missing handle-opt-missing-value
   :positional  handle-positional})

(defn process-tokens
  "Process token stream using pattern matching.

   Uses match-first to try patterns in order and dispatch to handlers."
  [schema tokens]
  (let [patterns (make-patterns schema)]
    (loop [tokens (vec tokens)
           result {:flags #{} :options {} :positional [] :errors []}
           after-end-opts? false]
      (if (empty? tokens)
        result
        (if after-end-opts?
          ;; After --, everything is positional
          (let [tok (first tokens)]
            (recur (vec (rest tokens))
                   (update result :positional conj (reconstruct-token tok))
                   true))
          ;; Try patterns in order, dispatch to matching handler
          (if-let [[case-key match] (match-first patterns tokens)]
            (let [handler (get handlers case-key)
                  [new-result remaining] (handler match result schema)]
              (recur (vec remaining) new-result (= case-key :end-opts)))
            ;; No pattern matched - unknown token
            (recur (vec (rest tokens))
                   (update result :errors conj {:error :unknown-token :token (first tokens)})
                   after-end-opts?)))))))

(comment
  ;; The pattern-based approach is more declarative:
  ;; - Each pattern describes WHAT to match
  ;; - Each handler describes HOW to process the match
  ;; - The loop just orchestrates pattern matching

  ;; Test the new process-tokens
  (process-tokens example-schema
                  [{:type :short-opt :char \v}
                   {:type :short-opt :char \f}
                   {:type :positional :value "input.txt"}
                   {:type :long-opt :name "output" :value "out.txt"}])
  ;=> {:flags #{:v}, :options {:file "input.txt", :output "out.txt"},
  ;    :positional [], :errors []}
  )

;;=============================================================================
;; PART 7: Full Parser - Putting It All Together
;;=============================================================================

(defn parse-cli
  "Parse command-line arguments according to schema.

   Returns:
   {:flags #{:verbose :debug ...}     ;; Boolean flags
    :options {:file \"input.txt\" ...} ;; Options with values
    :positional [\"arg1\" \"arg2\"]    ;; Positional arguments
    :errors [...]}                     ;; Any parsing errors"
  [schema args]
  (->> args
       (mapv classify-token)        ;; 1. Classify each token
       (mapcat expand-short-opts)   ;; 2. Expand -abc to -a -b -c
       vec
       (process-tokens schema)))    ;; 3. Process token stream

(comment
  ;;-------------------------------------------------------------------
  ;; Basic flag parsing
  ;;-------------------------------------------------------------------
  (parse-cli example-schema ["-v" "--debug"])
  ;=> {:flags #{:v :debug}, :options {}, :positional [], :errors []}

  ;;-------------------------------------------------------------------
  ;; Options with values
  ;;-------------------------------------------------------------------
  (parse-cli example-schema ["-f" "input.txt" "--output=out.txt"])
  ;=> {:flags #{},
  ;    :options {:file "input.txt", :output "out.txt"},
  ;    :positional [],
  ;    :errors []}

  ;;-------------------------------------------------------------------
  ;; Combined short flags
  ;;-------------------------------------------------------------------
  (parse-cli example-schema ["-vd" "file.txt"])
  ;=> {:flags #{:v :d}, :options {}, :positional ["file.txt"], :errors []}

  ;;-------------------------------------------------------------------
  ;; Long option with space-separated value
  ;;-------------------------------------------------------------------
  (parse-cli example-schema ["--file" "input.txt" "--output" "out.txt"])
  ;=> {:flags #{},
  ;    :options {:file "input.txt", :output "out.txt"},
  ;    :positional [],
  ;    :errors []}

  ;;-------------------------------------------------------------------
  ;; End of options marker
  ;;-------------------------------------------------------------------
  (parse-cli example-schema ["--verbose" "--" "-not-a-flag" "--also-not"])
  ;=> {:flags #{:verbose},
  ;    :options {},
  ;    :positional ["-not-a-flag" "--also-not"],
  ;    :errors []}

  ;;-------------------------------------------------------------------
  ;; Mixed usage - realistic example
  ;;-------------------------------------------------------------------
  (parse-cli example-schema
             ["-v" "-f" "input.txt" "--output=result.json" "--dry-run" "extra-arg"])
  ;=> {:flags #{:v :dry-run},
  ;    :options {:file "input.txt", :output "result.json"},
  ;    :positional ["extra-arg"],
  ;    :errors []}

  ;;-------------------------------------------------------------------
  ;; Edge cases
  ;;-------------------------------------------------------------------
  ;; Empty args
  (parse-cli example-schema [])
  ;=> {:flags #{}, :options {}, :positional [], :errors []}

  ;; Only positional
  (parse-cli example-schema ["one" "two" "three"])
  ;=> {:flags #{}, :options {}, :positional ["one" "two" "three"], :errors []}

  ;; Unknown option produces error
  (parse-cli example-schema ["--unknown-opt"])
  ;=> {:flags #{}, :options {}, :positional [],
  ;    :errors [{:error :unknown-option, :token {:type :long-flag, :name "unknown-opt"}}]}
  )

;;=============================================================================
;; PART 8: Sequence Pattern Matching Power
;;=============================================================================
;;
;; The real power of flybot.pullable is in sequence pattern matching.
;; Let's explore various patterns for stream processing.

(comment
  ;;-------------------------------------------------------------------
  ;; Basic head + rest patterns (using plain symbols)
  ;;-------------------------------------------------------------------

  ;; Match first element and rest
  (p/query '[head ?rest*] [:a :b :c :d])
  ;=> {head :a, rest (:b :c :d)}

  ;; Match first N elements
  (p/query '[a b c ?rest*] [:x :y :z 1 2 3])
  ;=> {a :x, b :y, c :z, rest (1 2 3)}

  ;; Greedy vs lazy: get all but last
  (p/query '[?init*! last] [:a :b :c :d])
  ;=> {init (:a :b :c), last :d}

  ;;-------------------------------------------------------------------
  ;; Predicates on elements
  ;;-------------------------------------------------------------------

  ;; Match if first element satisfies predicate
  (p/query '[(?x keyword?) ?rest*] [:flag "value" :other])
  ;=> {x :flag, rest ("value" :other)}

  ;; No match if predicate fails
  (p/query '[(?x keyword?) ?rest*] ["not-a-keyword" :other])
  ;=> nil

  ;;-------------------------------------------------------------------
  ;; Token stream matching with predicates
  ;;-------------------------------------------------------------------

  ;; Match short-opt followed by positional (option + value)
  (p/query '[(opt #(= :short-opt (:type %)))
             (val #(= :positional (:type %)))
             ?rest*]
           [{:type :short-opt :char \f}
            {:type :positional :value "file.txt"}
            {:type :long-flag :name "verbose"}])
  ;=> {opt {:type :short-opt, :char \f}
  ;    val {:type :positional, :value "file.txt"}
  ;    rest ({:type :long-flag, :name "verbose"})}

  ;; Match any flag (short or long)
  (p/query '[(flag #(#{:short-opt :long-flag} (:type %))) ?rest*]
           [{:type :long-flag :name "verbose"}
            {:type :positional :value "file.txt"}])
  ;=> {flag {:type :long-flag, :name "verbose"}
  ;    rest ({:type :positional, :value "file.txt"})}

  ;;-------------------------------------------------------------------
  ;; Using :filter to collect specific elements
  ;;-------------------------------------------------------------------
  ;; :filter collects all elements matching a predicate
  ;; Works great with simple predicates like even?

  (p/query (list '? :seq [(list '? :filter even? 'evens)])
           [1 2 3 4 5 6])
  ;=> {evens [2 4 6]}

  (p/query (list '? :seq [(list '? :filter string? 'strings)])
           [1 "hello" 2 "world" 3])
  ;=> {strings ["hello" "world"]}

  ;;-------------------------------------------------------------------
  ;; Using :first to find specific element
  ;;-------------------------------------------------------------------
  ;; :first finds the first element matching a predicate

  (p/query (list '? :seq [(list '? :first even? 'first-even)])
           [1 3 5 6 7 8])
  ;=> {first-even 6}

  (p/query (list '? :seq [(list '? :first string? 'first-str)])
           [1 2 "found" 3 "other"])
  ;=> {first-str "found"}
  )

;;=============================================================================
;; PART 9: Pattern-Based Token Consumer
;;=============================================================================
;;
;; Here's a more pattern-driven approach to consuming tokens.

(comment
  ;; Using patterns to match option + value pair
  (p/query '[(opt #(= :short-opt (:type %)))
             (val #(= :positional (:type %)))
             ?rest*]
           [{:type :short-opt :char \f}
            {:type :positional :value "file.txt"}
            {:type :long-flag :name "verbose"}])
  ;=> {opt {:type :short-opt, :char \f}
  ;    val {:type :positional, :value "file.txt"}
  ;    rest ({:type :long-flag, :name "verbose"})}

  ;; Using patterns to match a flag followed by anything
  (p/query '[(flag #(= :long-flag (:type %))) ?rest*]
           [{:type :long-flag :name "verbose"}
            {:type :positional :value "file.txt"}])
  ;=> {flag {:type :long-flag, :name "verbose"}
  ;    rest ({:type :positional, :value "file.txt"})}
  )

;;=============================================================================
;; PART 10: Key Takeaways & Suggested Library Enhancements
;;=============================================================================
;;
;; This demo showcased several powerful pattern matching features:
;;
;; 1. SEQUENCE PATTERNS - The core strength
;;    - Match head + rest: [head ?rest*]
;;    - Lazy vs greedy: [?init*! last] gets all but last
;;    - Match with predicates: [(x pred?) ?rest*]
;;
;; 2. PREDICATE MATCHING
;;    - Filter by type: #(= :short-opt (:type %))
;;    - Combine predicates: #(#{:short-opt :long-flag} (:type %))
;;    - Set predicates: (? :pred #{:a :b}) for membership test
;;    - Map predicates: (? :pred {:k v}) for KEY lookup (not submap)
;;
;; 3. STREAM PROCESSING PATTERNS
;;    - :filter - Collect all elements matching predicate
;;    - :first - Find first matching element
;;    - Option+value pairs with consecutive element matching
;;
;; 4. DECLARATIVE OVER IMPERATIVE
;;    - Describe WHAT to match, not HOW
;;    - Pattern matching handles the control flow
;;    - Easy to add new token types or parsing rules
;;
;; 5. COMPOSABLE MATCHERS
;;    - Build complex parsers from simple patterns
;;    - Each pattern handles one concern
;;    - Combine with p/query for readable parsing code
;;
;; 6. NEW PLAIN SYMBOL SYNTAX
;;    - Use plain symbols for variables: [head ?tail*] or {name age}
;;    - Use _ for wildcard: [first _ third]
;;    - Use ''sym for literal symbols: {type ''response}
;;    - More Clojure-like and readable
;;
;;-----------------------------------------------------------------------------
;; SUGGESTED LIBRARY ENHANCEMENTS
;;-----------------------------------------------------------------------------
;;
;; 1. SUBMAP PREDICATE - Match maps by field values
;;    Current: #(= :end-opts (:type %))   ; verbose anonymous function
;;    Suggested: (? :submap {:type :end-opts})  ; match if value contains k/v pairs
;;    Or: (? :pred= {:type :end-opts})    ; field equality check
;;
;;    This would make token matching much cleaner:
;;      [(list 'tok #(= :flag (:type %))) '?rest*]  ; current
;;      ['(tok (? :submap {:type :flag})) '?rest*]  ; proposed
;;
;; 2. :match-case WITH PATTERN DATA
;;    Current: :match-case requires pre-compiled matchers (core namespace)
;;    Suggested: Allow pattern data in :match-case, compile on the fly
;;
;;    Would enable single-pattern token stream parsing:
;;      (? :match-case
;;         [:end-opts   [(end (? :submap {:type :end-opts})) ?rest*]
;;          :flag       [(opt (? :submap {:type :flag})) ?rest*]
;;          :positional [(pos (? :submap {:type :positional})) ?rest*]]
;;         'which)
;;
;; 3. SHORTHAND FOR FIELD PREDICATES
;;    Current: #(#{:a :b} (:type %))
;;    Suggested: {:type #{:a :b}} as pattern (not key lookup)
;;    Or: (? :field :type #{:a :b})  ; check (:type val) is in set
;;
;; The combination of sequence matching, predicates, and filtering makes
;; flybot.pullable ideal for stream processing and parsing tasks!
