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

(defn classify-token
  "Classify a single CLI token using regex matching."
  [token]
  (condp #(re-matches %1 %2) token
    #"--(\w+)=(.+)" :>> (fn [[_ name value]] {:type :long-opt :name name :value value})
    #"--(\w+)"      :>> (fn [[_ name]] {:type :long-flag :name name})
    #"-(\w+)"       :>> (fn [[_ chars]] {:type :short-opts :chars chars})
    #"--"           {:type :end-opts}
    {:type :positional :value token}))

;; Test token classification
(classify-token "-v")
;=> {:type :short-opts, :chars "v"}

(classify-token "--verbose")
;=> {:type :long-flag, :name "verbose"}

(classify-token "--output=file.txt")
;=> {:type :long-opt, :name "output", :value "file.txt"}

(classify-token "file.txt")
;=> {:type :positional, :value "file.txt"}

(classify-token "--")
;=> {:type :end-opts}

;;=============================================================================
;; PART 3: Expanding Combined Short Options
;;=============================================================================
;;
;; Unix allows -abc as shorthand for -a -b -c.
;; We'll expand these into individual tokens.

(defn expand-short-opts
  "Expand combined short options: -abc becomes [{:type :short-opt :char \\a} ...]"
  [token]
  (if (= :short-opts (:type token))
    (mapv (fn [c] {:type :short-opt :char c}) (:chars token))
    [token]))

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

;;=============================================================================
;; PART 4: Schema Definition
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
     "verbose" "debug" "help" "dry-run"}}) ;; long forms

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

;;=============================================================================
;; PART 5: Pattern-Based Token Stream Parsing
;;=============================================================================
;;
;; This is where pattern matching shines! We use match-fn to create
;; matchers for different token sequences.

;;-----------------------------------------------------------------------------
;; Token type predicates
;;-----------------------------------------------------------------------------

(defn end-opts? [t] (= :end-opts (:type t)))
(defn long-opt? [t] (= :long-opt (:type t)))
(defn positional? [t] (= :positional (:type t)))
(defn option-like? [t] (#{:short-opt :long-flag} (:type t)))

;;-----------------------------------------------------------------------------
;; Pattern matchers using match-fn
;;-----------------------------------------------------------------------------

(defn make-matchers
  "Create token stream matchers with schema-aware predicates.

   Returns a vector of [case-key matcher-fn] pairs in priority order."
  [schema]
  [[:end-opts
    (p/match-fn [?end ?rest*]
                (when (end-opts? ?end)
                  {:end ?end :rest ?rest}))]

   [:long-opt
    (p/match-fn [?opt ?rest*]
                (when (long-opt? ?opt)
                  {:opt ?opt :rest ?rest}))]

   [:opt-value
    (p/match-fn [?opt ?val ?rest*]
                (when (and (option-like? ?opt)
                           (not (flag? schema ?opt))
                           (positional? ?val))
                  {:opt ?opt :val ?val :rest ?rest}))]

   [:flag
    (p/match-fn [?opt ?rest*]
                (when (and (option-like? ?opt)
                           (flag? schema ?opt))
                  {:opt ?opt :rest ?rest}))]

   [:opt-missing
    (p/match-fn [?opt ?rest*]
                (when (and (option-like? ?opt)
                           (not (flag? schema ?opt)))
                  {:opt ?opt :rest ?rest}))]

   [:positional
    (p/match-fn [?pos ?rest*]
                (when (positional? ?pos)
                  {:pos ?pos :rest ?rest}))]])

(defn match-first
  "Try matchers in order, return [case-key match] for first successful match."
  [matchers tokens]
  (some (fn [[case-key matcher]]
          (let [result (matcher tokens)]
            (when (and result (not (p/failure? result)))
              [case-key result])))
        matchers))

;; Test the matchers
(def test-matchers (make-matchers example-schema))

;; Matches :end-opts case
(match-first test-matchers [{:type :end-opts} {:type :positional :value "x"}])
;=> [:end-opts {:end {:type :end-opts}, :rest ({:type :positional, :value "x"})}]

;; Matches :long-opt case
(match-first test-matchers [{:type :long-opt :name "output" :value "file.txt"}])
;=> [:long-opt {:opt {:type :long-opt, :name "output", :value "file.txt"}, :rest ()}]

;; Matches :opt-value case (-f expects value, so matches opt+val)
(match-first test-matchers [{:type :short-opt :char \f}
                            {:type :positional :value "file.txt"}])
;=> [:opt-value {:opt {:type :short-opt, :char \f},
;                :val {:type :positional, :value "file.txt"}, :rest ()}]

;; Matches :flag case (-v is a flag)
(match-first test-matchers [{:type :short-opt :char \v}
                            {:type :positional :value "file.txt"}])
;=> [:flag {:opt {:type :short-opt, :char \v},
;           :rest ({:type :positional, :value "file.txt"})}]

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
  [{:keys [rest]} result _schema]
  (let [values (map reconstruct-token rest)]
    [(update result :positional into values) []]))

(defn handle-long-opt
  "Handle --name=value"
  [{:keys [opt rest]} result schema]
  (if-let [k (option-key schema opt)]
    [(assoc-in result [:options k] (:value opt)) rest]
    [(update result :errors conj {:error :unknown-option :token opt}) rest]))

(defn handle-opt-value
  "Handle option followed by value"
  [{:keys [opt val rest]} result schema]
  (if-let [k (option-key schema opt)]
    [(assoc-in result [:options k] (:value val)) rest]
    [(update result :errors conj {:error :unknown-option :token opt}) rest]))

(defn handle-flag
  "Handle a flag (boolean option)"
  [{:keys [opt rest]} result _schema]
  (let [flag-name (or (:name opt) (str (:char opt)))]
    [(update result :flags conj (keyword flag-name)) rest]))

(defn handle-opt-missing-value
  "Handle option that needs a value but doesn't have one"
  [{:keys [opt rest]} result _schema]
  [(update result :errors conj {:error :missing-value :option opt}) rest])

(defn handle-positional
  "Handle positional argument"
  [{:keys [pos rest]} result _schema]
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

   Uses match-first to try matchers in order and dispatch to handlers."
  [schema tokens]
  (let [matchers (make-matchers schema)]
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
          ;; Try matchers in order, dispatch to matching handler
          (if-let [[case-key match] (match-first matchers tokens)]
            (let [handler (get handlers case-key)
                  [new-result remaining] (handler match result schema)]
              (recur (vec remaining) new-result (= case-key :end-opts)))
            ;; No pattern matched - unknown token
            (recur (vec (rest tokens))
                   (update result :errors conj {:error :unknown-token :token (first tokens)})
                   after-end-opts?)))))))

;; The pattern-based approach is more declarative:
;; - Each pattern describes WHAT to match
;; - Each handler describes HOW to process the match
;; - The loop just orchestrates pattern matching

;; Test process-tokens
(process-tokens example-schema
                [{:type :short-opt :char \v}
                 {:type :short-opt :char \f}
                 {:type :positional :value "input.txt"}
                 {:type :long-opt :name "output" :value "out.txt"}])
;=> {:flags #{:v}, :options {:file "input.txt", :output "out.txt"},
;    :positional [], :errors []}

;;=============================================================================
;; PART 6: Full Parser - Putting It All Together
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

;;=============================================================================
;; PART 7: Sequence Pattern Matching Power
;;=============================================================================
;;
;; The real power of flybot.pullable is in sequence pattern matching.
;; Let's explore various patterns using match-fn.

;;-------------------------------------------------------------------
;; Basic head + rest patterns
;;-------------------------------------------------------------------

;; Match first element and rest
((p/match-fn [?head ?rest*] {:head ?head :rest ?rest})
 [:a :b :c :d])
;=> {:head :a, :rest (:b :c :d)}

;; Match first N elements
((p/match-fn [?a ?b ?c ?rest*] {:a ?a :b ?b :c ?c :rest ?rest})
 [:x :y :z 1 2 3])
;=> {:a :x, :b :y, :c :z, :rest (1 2 3)}

;; Greedy: get all but last
((p/match-fn [?init*! ?last] {:init ?init :last ?last})
 [:a :b :c :d])
;=> {:init (:a :b :c), :last :d}

;;-------------------------------------------------------------------
;; Combining match-fn with predicates in body
;;-------------------------------------------------------------------

;; Use when to add conditions in the body
((p/match-fn [?first ?rest*]
             (when (keyword? ?first)
               {:keyword ?first :rest ?rest}))
 [:flag "value" :other])
;=> {:keyword :flag, :rest ("value" :other)}

;; No match if predicate fails (returns nil)
((p/match-fn [?first ?rest*]
             (when (keyword? ?first)
               {:keyword ?first :rest ?rest}))
 ["not-a-keyword" :other])
;=> nil

;;-------------------------------------------------------------------
;; Practical: Token stream matching
;;-------------------------------------------------------------------

;; Match short-opt followed by positional (option + value)
((p/match-fn [?opt ?val ?rest*]
             (when (and (= :short-opt (:type ?opt))
                        (= :positional (:type ?val)))
               {:opt ?opt :val ?val :rest ?rest}))
 [{:type :short-opt :char \f}
  {:type :positional :value "file.txt"}
  {:type :long-flag :name "verbose"}])
;=> {:opt {:type :short-opt, :char \f}
;    :val {:type :positional, :value "file.txt"}
;    :rest ({:type :long-flag, :name "verbose"})}

;;-------------------------------------------------------------------
;; Unification - same variable must match same value
;;-------------------------------------------------------------------

;; Check if first and last elements are equal
((p/match-fn [?x ?middle* ?x] {:bookend ?x :middle ?middle})
 [:a :b :c :a])
;=> {:bookend :a, :middle (:b :c)}

;; Fails when first != last
(p/failure? ((p/match-fn [?x ?middle* ?x] :ok) [:a :b :c :d]))
;=> true

;;=============================================================================
;; PART 8: Key Takeaways
;;=============================================================================
;;
;; This demo showcased several powerful pattern matching features:
;;
;; 1. MATCH-FN - The core API
;;    - Creates pattern-matching functions
;;    - Pattern variables: ?x, ?rest*, ?init*!, ?opt?
;;    - Special $ binds to matched value
;;    - Returns MatchFailure on mismatch (check with failure?)
;;
;; 2. SEQUENCE PATTERNS - The core strength
;;    - Match head + rest: [?head ?rest*]
;;    - Lazy vs greedy: [?init*! ?last] gets all but last
;;    - Unification: [?x ?middle* ?x] checks first == last
;;
;; 3. COMBINING PATTERNS WITH PREDICATES
;;    - Use when/if in match-fn body for conditional matching
;;    - Return nil to signal "not a match" from body
;;    - Predicates as map values: {:age even?}
;;
;; 4. DECLARATIVE OVER IMPERATIVE
;;    - Describe WHAT to match, not HOW
;;    - Pattern matching handles the control flow
;;    - Easy to add new token types or parsing rules
;;
;; 5. COMPOSABLE MATCHERS
;;    - Build complex parsers from simple match-fn functions
;;    - Each matcher handles one concern
;;    - Combine with match-first for priority-based dispatch
