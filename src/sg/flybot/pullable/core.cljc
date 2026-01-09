(ns sg.flybot.pullable.core
  "Core pattern matching engine.

   This namespace provides the complete pattern matching implementation:
   - Match result types (ValMatchResult, MatchFailure)
   - Matcher primitives (mpred, mval, mmap, mseq, mor, etc.)
   - Pattern DSL and compilation (ptn->matcher, defmatcher, core-rules)
   - Public API (query, match-result, match!, CompiledMatcher)"
  (:require
   [clojure.walk :as walk]
   [clojure.zip :as zip]))

;;=============================================================================
;; SECTION 1: Match Result Types
;;=============================================================================

;;# Matcher and Match Result
;;
;; A matcher is a function that takes a match result (mr) and returns an mr,
;; or a MatchFailure if not matching.
;;
;; A match result contains `:val` (the current value) and `:vars` (a map of
;; variable bindings accumulated during matching).
;;
;;## Terminology
;;
;; | Abbrev | Meaning                    | Description                                    |
;; |--------|----------------------------|------------------------------------------------|
;; | mr     | match result               | ValMatchResult or MatchFailure                 |
;; | vmr    | ValMatchResult constructor | Creates a ValMatchResult with :val and :vars   |
;; | zmr    | zipper match result        | An mr where :val is a zipper (sequence matchers)|
;;
;;## Naming Conventions
;;
;; | Prefix | Category                   | Examples                                       |
;; |--------|----------------------------|------------------------------------------------|
;; | m-     | matcher constructors       | mpred, mval, mmap, mseq, mor, mvar, mchain     |
;; | mz-    | zipper sub-matchers        | mzone, mzrepeat, mzfilter, mzfirst, mzoption   |
;; | -rule  | pattern compilation rules  | core-rule, map-rule, vector-rule               |

(defprotocol IMatchResult
  (-fapply [mr f]
    "apply `f` to `mr` with the argument of current mr, returns a mr of the result as `:val` of the mr")
  (-bind [mr sym v]
    "bind symbol `sym` with `v` in the result"))

;;## Match Failure
;;
;; A MatchFailure represents a failed match with diagnostic information.
;; - reason: Human-readable failure message
;; - matcher-type: Keyword identifying the matcher (e.g., :pred, :map)
;; - path: Vector of keys/indices showing location in data
;; - value: The value that failed to match
;; - depth: Integer tracking match progress (for "best failure" selection)

(defrecord MatchFailure [reason matcher-type path value depth])

(defn failure?
  "Returns true if x is a MatchFailure"
  [x]
  (instance? MatchFailure x))

(defn fail
  "Create a failure with given reason, matcher type, and value"
  ([reason mt value]
   (MatchFailure. reason mt [] value 0))
  ([reason mt value path depth]
   (MatchFailure. reason mt path value depth)))

(defn deeper-failure
  "Return the failure with greater depth (got further before failing)"
  [f1 f2]
  (cond
    (nil? f1) f2
    (nil? f2) f1
    (> (:depth f1) (:depth f2)) f1
    :else f2))

(defn nest-failure
  "Add path segment and increment depth for nested matching"
  [failure segment]
  (-> failure
      (update :path #(into [segment] %))
      (update :depth inc)))

;; Forward declare for ValMatchResult implementation
(declare vmr)

(defrecord ValMatchResult [val vars]
  IMatchResult
  (-fapply [this f]
    (let [v (f this)]
      (if (= v ::none)
        (fail "fapply returned none" :fapply val)
        (ValMatchResult. v vars))))
  (-bind [this sym v]
    (let [old-v (get vars sym ::not-found)]
      (condp = old-v
        ::not-found (ValMatchResult. val (assoc vars sym v))
        v this
        (fail (str "binding conflict for '" sym "': had " old-v ", got " v) :bind val)))))

(defn vapply
  "apply `f` to a `mr`'s `:val`"
  [mr f]
  (-fapply mr (comp f :val)))

(defn fit
  "returns the `mr` if `pred` to its `:val` is truthy, otherwise returns a MatchFailure"
  [mr pred]
  (let [val (:val mr)]
    (if (pred val)
      mr
      (fail (str "predicate " pred " failed for value " (pr-str val)) :pred val))))

(defn vmr
  "returns a mr with `val` and `vars`"
  ([val]
   (vmr val {}))
  ([val vars]
   (ValMatchResult. val vars)))

(defn merge-vars
  "Merge variables from `other` into `mr`"
  [mr other]
  (if-let [vo (:vars other)]
    (vmr (:val mr) (merge (:vars mr) vo))
    mr))

^:rct/test
(comment
  ;; vapply transforms val by applying f
  (vapply (vmr 3) inc) ;=>> {:val 4}
  ;; -bind adds variable binding to vars
  (-bind (vmr 3) 'a 4) ;=>> {:vars {'a 4}}
  ;; fit returns mr when predicate succeeds
  (fit (vmr 3) odd?) ;=>> {:val 3}
  ;; fit returns MatchFailure when predicate fails
  (fit (vmr 4) odd?) ;=>> failure?
  )

;;=============================================================================
;; SECTION 2: Matcher Primitives
;;=============================================================================

;;## Implementation of matchers
;;A matcher is a function take a mr and returns a mr, if not match, returns nil

(defn matcher
  "Create a matcher function with metadata tracking its type"
  [mt f & {:as other}]
  (with-meta f (merge other {::matcher-type mt})))

(defn matcher-type
  "Get the matcher type from a matcher's metadata"
  [x]
  (some-> x meta ::matcher-type))

(defn mpred
  "a matcher if `pred` on mr's `:val` success"
  [pred]
  (matcher :pred #(fit % pred)))

(defn mval
  "a matcher that succeeds when mr's value equals v"
  [v]
  (matcher :val
           (fn [mr]
             (let [actual (:val mr)]
               (if (= v actual)
                 mr
                 (fail (str "value mismatch: expected " (pr-str v) ", got " (pr-str actual)) :val actual))))))

(def wildcard
  "A matcher that matches anything"
  (matcher :wildcard identity))

(defn msub
  "substitute mr's `:val` with `f` applies to it"
  [f]
  (matcher :sub #(vapply % f)))

(defn mf
  "A matcher that applies `f` to the full match result (not just :val).
   `f` receives the mr and its return value becomes the new :val.
   Use msub when you only need to transform :val."
  [f]
  (matcher :f #(-fapply % f)))

(defn mvar
  "bind a `child` matcher's `:val` to a `sym`"
  [sym child]
  (with-meta
    (fn [mr]
      (let [mr' (child mr)]
        (if (failure? mr')
          mr'
          (-bind mr' sym (:val mr')))))
    (meta child)))

^:rct/test
(comment
  ;; mpred succeeds when predicate passes, fails otherwise
  (map (mvar 'a (mpred even?)) [(vmr 4) (vmr 3)]) ;=>>
  [{:val 4 :vars {'a 4}} failure?]
  ;; mval succeeds on exact value match
  ((mval 3) (vmr 3)) ;=>> {:val 3}
  ;; mval fails on value mismatch
  ((mval 3) (vmr 5)) ;=>> failure?
  ;; msub transforms val by applying function
  ((msub inc) (vmr 3)) ;=>> {:val 4}
  ;; mf transforms using full match result (access to both val and vars)
  ((mf (fn [{:keys [vars val]}] (+ val (vars 'a)))) (vmr 3 {'a 2})) ;=>> {:val 5}
  )

(def find-first
  "returns the first matched element"
  (comp first filter))

(defn regex?
  "Returns true if x is a regex pattern"
  [x]
  #?(:clj (instance? java.util.regex.Pattern x)
     :cljs (regexp? x)))

(defn mor
  "a matcher tests `matchers` and returns the first success, or the best (deepest) failure"
  [matchers]
  (matcher :or
           (fn [mr]
             (loop [[m & rest] matchers
                    best-fail nil]
               (if m
                 (let [result (m mr)]
                   (if (failure? result)
                     (recur rest (deeper-failure best-fail result))
                     result))
                 (or best-fail (fail "no matchers to try" :or (:val mr))))))))

(defn mnor
  "a matcher tests sequentially `kv-matchers`, which is a key to a matcher,
  bind the successful key to `sym`"
  [kv-matchers sym]
  (matcher :nor
           (fn [mr]
             (loop [kvs (partition 2 kv-matchers)
                    best-fail nil]
               (if-let [[[k mch] & rest] (seq kvs)]
                 (let [result (mch mr)]
                   (if (failure? result)
                     (recur rest (deeper-failure best-fail result))
                     (if sym (-bind result sym k) result)))
                 (or best-fail (fail "no matchers matched" :nor (:val mr))))))))

^:rct/test
(comment
  ;; mor returns first successful match (short-circuits)
  (map (mor [(mpred neg?) (mpred even?)]) [(vmr -1) (vmr 2) (vmr 3)]) ;=>>
  [{} {} failure?]
  ;; mnor tries matchers in order, binds key of first success to symbol
  ((mnor [0 (mpred even?) 1 (mpred odd?)] 'b) (vmr 3)) ;=>> {:vars {'b 1}}
  )

;;## Map matcher

(defn mmap
  "a matcher matches on `matcher-map`, which is a key to matcher map"
  [matcher-map]
  (matcher :map
           (fn [mr]
             (let [m (:val mr)]
               (if-not (map? m)
                 (fail (str "expected map, got " (type m)) :map m)
                 (reduce
                  (fn [mr' [k mch]]
                    (let [result (mch (vmr (get m k)))]
                      (if (failure? result)
                        (reduced (nest-failure result k))
                        (-> mr' (update :val conj [k (:val result)]) (merge-vars result)))))
                  mr matcher-map))))))

^:rct/test
(comment
  ;; mmap succeeds when all key matchers succeed, collects vars
  (def mm (mmap {:a (mvar 'a (mval 4)) :b (mpred even?)}))
  (mm (vmr {:a 4 :b 0})) ;=>> {:val {:a 4 :b 0} :vars {'a 4}}
  ;; mmap fails when any key matcher fails
  (mm (vmr {:a 4 :b 3})) ;=>> failure?
  )

;;## Sequence matchers
;;
;;The key function is `mseq` which contains sub-matchers, these sub-matchers
;;use clojure.zip's zipper as the `:val` of the mr, the convention is `zmr`,
;;The name convention for this kind of sub-matchers starting from `mz`, rather than `m`

(defn zip-advance
  "Advance zipper to next sibling. Returns end-marker if no more siblings.
   Unlike zip/next, this does NOT descend into nested sequences.
   The end marker preserves the tree root for later reconstruction."
  [z]
  (if-let [right (zip/right z)]
    right
    ;; No more siblings - create end marker that preserves the tree
    ;; Store the root in metadata so zip/root equivalent can be retrieved
    (with-meta [nil :end] {::zip-root (zip/root z)})))

(defn mzone
  "A sub-matcher means a single value matches by the `child` matcher"
  [child]
  (matcher :subseq
           (fn [zmr]
             (let [zip (:val zmr)]
               (if (zip/end? zip)
                 (fail "unexpected end of sequence" :subseq nil)
                 (let [node (zip/node zip)
                       result (child (assoc zmr :val node))]
                   (if (failure? result)
                     result
                     (-> zmr
                         (vapply (fn [z] (let [nv (:val result)] (-> (if (= node nv) z (zip/replace z nv)) zip-advance))))
                         (merge-vars result)))))))))

^:rct/test
(comment
  ;; mzone matches single element in zipper sequence
  ((mzone (mvar 'a (mval 3))) (vmr (-> (zip/vector-zip [3 4]) zip/down))) ;=>> {:vars {'a 3}}
  ;; mzone fails at end of sequence (no element to match)
  ((mzone (mvar 'a (mval 3))) (vmr [nil :end])) ;=>> failure?
  )

(defn branching
  "branching the `coll` on elements fits `pred`, then apply `f` to the element itself and its
  succeeding elements as a seq"
  [pred f coll]
  (loop [fst [] lst (reverse coll)]
    (let [[fst' [h & lst']] (split-with (complement pred) lst)]
      (if h
        (let [v (f h (reverse fst'))]
          (recur (concat fst [v]) lst'))
        (reverse (concat fst lst))))))

^:rct/test
(comment
  ;; branching groups elements by predicate, applies f to each group
  (def bf (partial branching #(zero? (mod % 5)) #(apply str % %2)))
  ;; branch at 5, group [5 6 7]
  (bf (range 1 8)) ;=> [1 2 3 4 "567"]
  ;; no branch point found - returns input unchanged
  (bf (range 1 3)) ;=> [1 2]
  ;; multiple branch points
  (bf [1 5 6 8 10 11]) ;=> [1 "568" "1011"]
  )

(defn- optional [x] (some-> x meta ::optional))

(defn mzsubseq
  "a sub-matcher can contains `sub-matchers`, bind the result to `sym` if not nil,
   if a `len` is specified, bind the part of `len`"
  ([sub-matchers sym]
   (mzsubseq sub-matchers sym nil))
  ([sub-matchers sym len]
   (let [children (branching optional (fn [c nxt] ((optional c) c nxt)) sub-matchers)]
     (matcher :subseq
              (fn [zmr]
                (let [result (reduce (fn [[zmr' acc idx] child]
                                       (let [zv (:val zmr')
                                             child-result (child zmr')]
                                         (if (failure? child-result)
                                           (reduced (nest-failure child-result idx))
                                           [child-result
                                            (cond-> acc (not (zip/end? zv)) (conj (zip/node zv)))
                                            (inc idx)])))
                                     [zmr [] 0] children)]
                  (if (failure? result)
                    result
                    (let [[rslt vals _] result]
                      (cond-> rslt sym (-bind sym (cond->> vals (some? len) (take len))))))))))))

(defn mzoption
  "a sub-matcher matches to `child` optionally"
  ([child]
   (mzoption child false))
  ([child lazy?]
   (matcher :subseq
            child
            ::optional (fn [c nxt]
                         (let [m1 (mzsubseq (cons (mzone c) nxt) nil)
                               m0 (mzsubseq nxt nil)]
                           (mor (if lazy? [m0 m1] [m1 m0])))))))

(defn- expand-optionals
  "Recursively expand optional sub-matchers into proper alternatives.
   Each optional is expanded with its successors (including expanded optionals) as nxt."
  [sub-matchers]
  (if (empty? sub-matchers)
    []
    (let [[head & tail] sub-matchers
          expanded-tail (expand-optionals tail)]
      (if-let [opt-fn (optional head)]
        ;; Head is optional - expand it with expanded tail as nxt
        [(opt-fn head expanded-tail)]
        ;; Head is not optional - keep it and continue
        (cons head expanded-tail)))))

(defn- zip-root*
  "Get the root of a zipper, handling our custom end-marker.
   For end-markers, retrieves root from metadata."
  [z]
  (if-let [root (::zip-root (meta z))]
    root
    (zip/root z)))

(defn mseq
  "a matcher matches a sequence by its `sub-matchers`"
  [sub-matchers]
  (let [children (expand-optionals sub-matchers)]
    (matcher :seq
             (fn [mr]
               (let [original (:val mr)]
                 (if-not (seqable? original)
                   (fail (str "expected seqable, got " (type original)) :seq original)
                   ;; Use zip/down to enter sequence. For empty seqs, down returns nil,
                   ;; so we use end-marker with original preserved in metadata.
                   (let [z (zip/seq-zip (seq original))
                         initial-z (or (zip/down z)
                                       (with-meta [nil :end] {::zip-root (seq original)}))
                         result (reduce
                                 (fn [zmr child]
                                   (let [child-result (child zmr)]
                                     (if (failure? child-result)
                                       (reduced child-result)
                                       child-result)))
                                 (vmr initial-z (:vars mr)) children)]
                     (if (failure? result)
                       result
                       ;; Preserve original collection type using empty + into
                       (vapply result (fn [z]
                                        (let [matched (zip-root* z)]
                                          (if (and (coll? original) (seqable? matched) (not (string? matched)))
                                            (let [e (empty original)]
                                              ;; Lists need reverse since conj prepends
                                              (if (list? e)
                                                (apply list matched)
                                                (into e matched)))
                                            matched))))))))))))

(def mterm
  "a matcher tests if it is the end of a seq"
  (matcher :term
           (fn [zmr]
             (let [zip (:val zmr)]
               (if (zip/end? zip)
                 zmr
                 (fail (str "expected end of sequence, but found: " (pr-str (zip/node zip))) :term (zip/node zip)))))))

^:rct/test
(comment
  ;; mseq fails on non-seqable input
  ((mseq []) (vmr 3)) ;=>> failure?
  ;; mterm succeeds at end, fails when elements remain
  (map mterm [(vmr [nil :end]) (vmr (-> (zip/vector-zip [3]) zip/down))]) ;=>>
  [identity failure?]
  ;; mseq single element match with termination check
  (map (mseq [(mzone (mvar 'a (mpred even?))) mterm]) [(vmr [0]) (vmr [2 4]) (vmr [1])]) ;=>>
  [{:vars {'a 0}} failure? failure?]
  ;; mseq handles nil elements correctly
  (map (mseq [(mzone (mpred nil?))]) [(vmr []) (vmr [nil])]) ;=>> [failure? {:val [nil]}]
  ;; mseq result can be bound via mvar
  ((mvar 'a (mseq [(mzone (mval 4))])) (vmr [4])) ;=>> {:vars {'a [4]}}
  ;; mseq supports value transformation via msub
  ((mseq [(mzone (mvar 'b (msub inc)))]) (vmr [3])) ;=>> {:val [4] :vars {'b 4}}
  ;; mzoption optional element - present vs absent
  (map (mseq [(mzoption (mvar 'a (mval 3))) (mzone (mval 0)) mterm]) [(vmr [3 0]) (vmr [0])]) ;=>>
  [{:vars {'a 3}} {}]
  ;; mzoption lazy mode prefers shorter match first
  (map (mseq [(mzoption (mvar 'a (mval 5)) true) (mzone (mvar 'b (mval 5))) mterm]) [(vmr [5]) (vmr [5 5])]) ;=>>
  [{:vars '{b 5}} {:vars '{a 5 b 5}}]
  ;; mzsubseq captures fixed-length subsequence
  ((mseq [(mzsubseq (repeat 3 (mzone (mpred even?))) 'a) (mzone (mpred odd?)) mterm]) (vmr [2 4 6 7])) ;=>>
  {:vars {'a [2 4 6]}})

(defn- try-match-elements
  "Try to match elements from zipper using child matcher.
   Returns [items final-zipper] where items are matched elements."
  [child zmr max-len]
  (loop [z (:val zmr)
         items []]
    (cond
      ;; Hit max limit
      (and max-len (>= (count items) max-len))
      [items z]
      ;; End of sequence
      (zip/end? z)
      [items z]
      ;; Try to match
      :else
      (let [node (zip/node z)
            result (child (assoc zmr :val node))]
        (if (failure? result)
          [items z]
          (recur (zip-advance z) (conj items (:val result))))))))

(defn- try-lengths
  "Try matching at different lengths in order, returns first success or failure.
   `lengths` is a seq of lengths to try (e.g., (range min max) or (range max min -1))."
  [zmr items lengths nxt-matcher sym]
  (let [start-z (:val zmr)]
    (loop [[len & more] lengths]
      (if (nil? len)
        (fail "no length worked" :repeat items)
        (let [z-at-len (nth (iterate zip-advance start-z) len)
              attempt (nxt-matcher (assoc zmr :val z-at-len))]
          (if (failure? attempt)
            (recur more)
            (cond-> (merge-vars zmr attempt)
              sym (-bind sym (take len items)))))))))

(defn mzrepeat
  "a sub-matcher repeats the `child` matcher with minimum length of `min-len`,
  Options:
   - `max-len` if it's length is flexible (nil means unbounded - uses actual sequence length)
   - `greedy?` if true, match maximum first then backtrack; if false, match minimum first
   - `sym` if we bind the whole sub-seq to a symbol"
  ([child min-len {:keys [max-len greedy? sym] :as opts}]
   (let [zc (mzone child)]
     (if (contains? opts :max-len)
       ;; Dynamic matching - determine length at runtime based on actual sequence
       (vary-meta zc assoc
                  ::optional (fn [_c nxt]
                               (let [nxt-matcher (mzsubseq nxt nil)]
                                 (matcher :repeat
                                          (fn [zmr]
                                            ;; Collect all matching elements up to max-len (nil = unbounded)
                                            (let [[items _final-z] (try-match-elements child zmr max-len)
                                                  item-count (count items)]
                                              (if (< item-count min-len)
                                                (fail (str "need at least " min-len " elements, got " item-count) :repeat items)
                                                ;; Generate length sequence based on greedy vs lazy
                                                (let [lengths (if greedy?
                                                                (range item-count (dec min-len) -1)
                                                                (range min-len (inc item-count)))]
                                                  (try-lengths zmr items lengths nxt-matcher sym)))))))))
       ;; Fixed length - match exactly min-len elements
       (mzsubseq (repeat min-len zc) sym)))))

^:rct/test
(comment
  ;; mzrepeat exact length matches exactly min-len elements
  (map (mseq [(mzrepeat (mpred even?) 3 {:sym 'a}) mterm]) [(vmr [8 10 12]) (vmr [8 10]) (vmr [8 10 12 4])]) ;=>>
  [{:vars {'a [8 10 12]}} failure? failure?]
  ;; mzrepeat variable length (lazy) matches minimum needed
  (map (mseq [(mzrepeat (mpred even?) 0 {:max-len 2 :sym 'a}) (mzone (mval 3)) mterm])
       [(vmr [3]) (vmr [0 3]) (vmr [0 2 3]) (vmr [0 2 4 3])]) ;=>>
  [{:vars '{a []}} {:vars '{a [0]}} {:vars '{a [0 2]}} failure?])

(defn mzfilter
  "A sub-matcher that filters remaining elements by `pred`, collecting matches.
   Consumes all remaining elements but only keeps those matching pred.
   Options:
   - `sym` if we bind the filtered elements to a symbol"
  [pred {:keys [sym]}]
  (matcher :subseq
           (fn [zmr]
             (loop [z (:val zmr)
                    filtered []]
               (if (zip/end? z)
                 (cond-> (assoc zmr :val z)
                   sym (-bind sym filtered))
                 (let [node (zip/node z)]
                   (recur (zip-advance z)
                          (if (pred node) (conj filtered node) filtered))))))))

^:rct/test
(comment
  ;; mzfilter collects elements matching predicate
  ((mseq [(mzfilter even? {:sym 'evens})]) (vmr [1 2 3 4 5 6])) ;=>> {:vars '{evens [2 4 6]}}
  ;; mzfilter returns empty when no matches
  ((mseq [(mzfilter neg? {:sym 'negs})]) (vmr [1 2 3])) ;=>> {:vars '{negs []}}
  ;; mzfilter combines with other sub-matchers
  ((mseq [(mzone (mvar 'first identity)) (mzfilter even? {:sym 'rest-evens})]) (vmr [1 2 3 4 5 6])) ;=>>
  {:vars '{first 1 rest-evens [2 4 6]}})

(defn mzfirst
  "A sub-matcher that finds the first element matching `pred`.
   Consumes all remaining elements but returns only the first match as :val.
   Fails if no element matches.
   Options:
   - `sym` if we bind the found element to a symbol"
  [pred {:keys [sym]}]
  (matcher :subseq
           (fn [zmr]
             (loop [z (:val zmr)]
               (if (zip/end? z)
                 (fail "no element matches predicate" :first nil)
                 (let [node (zip/node z)]
                   (if (pred node)
                     ;; Found it - create a fake end-zipper where zip/root returns the found value
                     ;; [value :end] is a zipper at end position, zip/root returns (node loc) = value
                     (cond-> (assoc zmr :val [node :end])
                       sym (-bind sym node))
                     (recur (zip-advance z)))))))))

^:rct/test
(comment
  ;; mzfirst finds first element matching predicate
  ((mseq [(mzfirst even? {:sym 'first-even})]) (vmr [1 3 5 6 7 8])) ;=>> {:val 6 :vars '{first-even 6}}
  ;; mzfirst fails when no element matches
  ((mseq [(mzfirst neg? {})]) (vmr [1 2 3])) ;=>> failure?
  ;; mzfirst combines with other sub-matchers
  ((mseq [(mzone (mvar 'head identity)) (mzfirst even? {:sym 'first-even})]) (vmr [1 3 4 5 6])) ;=>>
  {:val 4 :vars '{head 1 first-even 4}})

(defn mzcollect
  "A sub-matcher that collects all remaining elements matching pred into sym.
   Requires at least min-len elements (default 1)."
  ([pred sym] (mzcollect pred sym 1))
  ([pred sym min-len]
   (matcher :collect
            (fn [zmr]
              (loop [z (:val zmr)
                     items []]
                (if (zip/end? z)
                  (if (>= (count items) min-len)
                    (-bind (assoc zmr :val z) sym items)
                    (fail (str "need at least " min-len " elements, got " (count items)) :collect items))
                  (let [node (zip/node z)]
                    (if (pred node)
                      (recur (zip-advance z) (conj items node))
                      (if (>= (count items) min-len)
                        (-bind (assoc zmr :val z) sym items)
                        (fail (str "element doesn't match: " (pr-str node)) :collect node))))))))))

(defn mchain
  "Chain multiple matchers sequentially - output of one feeds into next"
  [children]
  (matcher :chain
           (fn [mr]
             (reduce (fn [mr' child]
                       (let [result (child mr')]
                         (if (failure? result)
                           (reduced result)
                           result)))
                     mr children))))

;;=============================================================================
;; SECTION 3: Named Variable Parsing
;;=============================================================================

(defn named-var?
  "Parse a symbol as a named pattern variable.
   Returns `[base-name flags]` when `v` matches pattern variable syntax.
   Supported forms:
   - ?x      basic binding
   - ?x?     optional (0 or 1)
   - ?x+     one or more (lazy)
   - ?x*     zero or more (lazy)
   - ?x+!    one or more (greedy)
   - ?x*!    zero or more (greedy)"
  [v]
  (when (symbol? v)
    (when-let [[_ n suffix greedy] (re-matches #"\?([\p{Alnum}-_]+)([\?\+\*]?)(\!?)" (name v))]
      [(when-not (= "_" n) (symbol n))
       (cond-> #{}
         (= suffix "?") (conj :optional)
         (= suffix "+") (conj :one-or-more)
         (= suffix "*") (conj :zero-or-more)
         (= greedy "!") (conj :greedy))])))

^:rct/test
(comment
  ;; named-var? parses basic binding ?x
  (named-var? '?x3) ;=>> ['x3 #{}]
  ;; named-var? parses optional ?x?
  (named-var? '?x?) ;=>> ['x #{:optional}]
  ;; named-var? parses one-or-more ?x+ (lazy)
  (named-var? '?x+) ;=>> ['x #{:one-or-more}]
  ;; named-var? parses zero-or-more ?x* (lazy)
  (named-var? '?x*) ;=>> ['x #{:zero-or-more}]
  ;; named-var? parses greedy one-or-more ?x+!
  (named-var? '?x+!) ;=>> ['x #{:one-or-more :greedy}]
  ;; named-var? parses greedy zero-or-more ?x*!
  (named-var? '?x*!) ;=>> ['x #{:zero-or-more :greedy}]
  ;; named-var? wildcard ?_ returns nil symbol
  (named-var? '?_) ;=>> [nil #{}]
  )

(defmacro substitute
  "Returns a function that substitutes named vars (like ?x, ?y) in `form`
   with values from `:vars` in the match result, then evaluates the form.

   Example:
     ((substitute '(+ 5 ?x ?y)) {:vars {'x 3 'y 0}}) ;=> 8"
  [form]
  (let [actual-form (if (and (seq? form) (= 'quote (first form)))
                      (second form)
                      form)]
    `(fn [{:keys [~'vars]}]
       ~(walk/postwalk
         (fn [x]
           (if-let [[nm _] (named-var? x)]
             `(get ~'vars '~nm)
             x))
         actual-form))))

(defn- build-form
  "Build code that constructs a form at runtime with var substitutions.
   Returns code that builds the data structure without evaluating it."
  [form vars-sym]
  (cond
    ;; Named variable - substitute with value from vars
    (and (symbol? form) (named-var? form))
    (let [[nm _] (named-var? form)]
      `(get ~vars-sym '~nm))

    ;; List - build with list*
    (list? form)
    `(list ~@(map #(build-form % vars-sym) form))

    ;; Vector - build with vector
    (vector? form)
    `(vector ~@(map #(build-form % vars-sym) form))

    ;; Map - build with hash-map
    (map? form)
    `(hash-map ~@(mapcat (fn [[k v]]
                           [(build-form k vars-sym)
                            (build-form v vars-sym)])
                         form))

    ;; Set - build with hash-set
    (set? form)
    `(hash-set ~@(map #(build-form % vars-sym) form))

    ;; Other values - quote them
    :else
    `'~form))

(defmacro substitute-form
  "Returns a function that substitutes named vars (like ?x, ?y) in `form`
   with values from `:vars` in the match result, returning the form without evaluating.

   Example:
     ((substitute-form '(* 2 ?x)) {:vars {'x 5}}) ;=> (* 2 5)"
  [form]
  (let [actual-form (if (and (seq? form) (= 'quote (first form)))
                      (second form)
                      form)
        vars-sym (gensym "vars")]
    `(fn [{:keys [~'vars]}]
       (let [~vars-sym ~'vars]
         ~(build-form actual-form vars-sym)))))

(defn substitute-vars
  "Runtime substitution: walk `form` and replace ?x symbols with values from `vars` map.
   Unlike `substitute-form` (a macro), this works with dynamic patterns at runtime.

   Example:
     (substitute-vars '(+ ?x ?x) {'x 5}) ;=> (+ 5 5)"
  [form vars]
  (walk/postwalk
   (fn [x]
     (if-let [[sym _] (named-var? x)]
       (get vars sym x)
       x))
   form))

^:rct/test
(comment
  ;; substitute evaluates form with vars replaced
  ((substitute '(+ 5 ?x ?y)) {:vars {'x 3 'y 0}}) ;=>> 8
  ;; substitute nested expressions
  ((substitute '(* ?a (+ ?b 1))) {:vars {'a 2 'b 3}}) ;=>> 8
  ;; substitute works with map literals
  ((substitute '{:result ?val}) {:vars {'val 42}}) ;=>> {:result 42}
  ;; substitute-form returns unevaluated form (data construction)
  ((substitute-form '(* 2 ?x)) {:vars {'x 5}}) ;=>> '(* 2 5)
  ((substitute-form '[?a ?b ?c]) {:vars {'a 1 'b 2 'c 3}}) ;=>> [1 2 3]
  ((substitute-form '{:sum ?x :product ?y}) {:vars {'x 10 'y 20}}) ;=>> {:sum 10 :product 20}
  ;; substitute-vars (runtime) replaces vars without evaluating
  (substitute-vars '(+ ?x ?x) {'x 5}) ;=>> '(+ 5 5)
  (substitute-vars '{:a ?a :b ?b} {'a 1 'b 2}) ;=>> {:a 1 :b 2}
  ;; substitute-vars leaves unbound vars as-is
  (substitute-vars '(f ?x ?y) {'x 10}) ;=>> '(f 10 ?y)
  )

(defn named-var->form
  "Transform a named-var symbol like ?x, ?x+, ?x* to a (? :named-var sym flags) form.
   Returns nil if x is not a named-var symbol."
  [x]
  (when-let [[sym flags] (named-var? x)]
    (list '? :named-var sym flags)))

^:rct/test
(comment
  ;; named-var->form transforms ?x to internal form
  (named-var->form '?x) ;=>> '(? :named-var x #{})
  ;; named-var->form preserves flags
  (named-var->form '?x+) ;=>> '(? :named-var x #{:one-or-more})
  )

;;=============================================================================
;; SECTION 4: Pattern DSL and Compilation
;;=============================================================================

(declare repeat-matcher)

(def ^:private matcher-specs* (atom {}))

(defmacro defmatcher
  "Define a matcher type specification.
   type - keyword like :pred, :val, etc.
   args - human-readable argument format string
   parse - matcher that validates and extracts arguments
   make - form with ?sym placeholders that are replaced with (get vars 'sym),
          or a (fn [vars] ...) for complex logic"
  [type args parse make]
  (let [make-form (if (and (seq? make) (= (first make) 'fn))
                    ;; Already a function form, use as-is
                    make
                    ;; Transform ?sym -> (get vars 'sym)
                    (let [transformed (walk/postwalk
                                       (fn [x]
                                         (if (and (symbol? x)
                                                  (clojure.string/starts-with? (name x) "?")
                                                  (not= (name x) "?"))
                                           `(get ~'vars '~(symbol (subs (name x) 1)))
                                           x))
                                       make)]
                      `(fn [~'vars] ~transformed)))]
    `(swap! matcher-specs* assoc ~type
            {:args ~args
             :parse ~parse
             :make ~make-form})))

(defn- resolve-var-args
  "Resolve $sym references in args using bound vars from match result.
   Uses $ prefix to avoid conflict with ?sym which is transformed to named-var patterns."
  [args bound-vars]
  (mapv (fn [arg]
          (if (and (symbol? arg)
                   (clojure.string/starts-with? (name arg) "$"))
            (get bound-vars (symbol (subs (name arg) 1)))
            arg))
        args))

(defn- parse-keyword-args
  "Parse keyword arguments from a flat sequence.
   Returns map of keyword->value pairs.
   Example: [:min 2 :max 4 :as x] -> {:min 2 :max 4 :as x}"
  [args]
  (loop [args args
         result {}]
    (if (empty? args)
      result
      (let [[k v & rest] args]
        (if (keyword? k)
          (recur rest (assoc result k v))
          result)))))

(defn- levenshtein
  "Calculate Levenshtein distance between two strings"
  [s1 s2]
  (let [len1 (count s1)
        len2 (count s2)]
    (cond
      (zero? len1) len2
      (zero? len2) len1
      :else
      (let [matrix (vec (for [i (range (inc len1))]
                          (vec (for [j (range (inc len2))]
                                 (cond (zero? i) j
                                       (zero? j) i
                                       :else 0)))))]
        (loop [i 1 matrix matrix]
          (if (> i len1)
            (get-in matrix [len1 len2])
            (recur (inc i)
                   (loop [j 1 matrix matrix]
                     (if (> j len2)
                       matrix
                       (let [cost (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1)
                             val (min (inc (get-in matrix [(dec i) j]))
                                      (inc (get-in matrix [i (dec j)]))
                                      (+ (get-in matrix [(dec i) (dec j)]) cost))]
                         (recur (inc j) (assoc-in matrix [i j] val))))))))))))

(defn- suggest-matcher-type
  "Suggest similar matcher type if the given one is unknown"
  [invalid-type valid-types]
  (let [invalid-name (name invalid-type)
        suggestions (->> valid-types
                         (map (fn [t] [t (levenshtein invalid-name (name t))]))
                         (filter (fn [[_ d]] (<= d 3)))
                         (sort-by second))]
    (when (seq suggestions)
      (ffirst suggestions))))

(defmatcher :pred "(? :pred <fn> [<args>...])"
  ;; Collect fn and all args
  (mzcollect (constantly true) 'all 1)
  (fn [vars]
    (let [[f & args] (get vars 'all)
          args (vec args)]
      (if (empty? args)
        ;; No extra args - original behavior
        (mpred f)
        ;; With args - substitute ?var and call f with args + current value
        (matcher :pred
                 (fn [mr]
                   (let [v (:val mr)
                         resolved-args (resolve-var-args args (:vars mr))
                         result (apply f (conj resolved-args v))]
                     (if result mr (fail "predicate failed" :pred v)))))))))

(defmatcher :val "(? :val <value>)"
  (mzone (mvar 'val identity))
  (mval ?val))

(defmatcher :map "(? :map <map>)"
  (mzone (mvar 'map (mpred map?)))
  (mmap ?map))

(defmatcher :one "(? :one <matcher>)"
  (mzone (mvar 'child (mpred matcher-type)))
  (mzone ?child))

(defmatcher :optional "(? :optional <matcher>)"
  (mzone (mvar 'child (mpred matcher-type)))
  (mzoption ?child))

(defmatcher :repeat "(? :repeat <matcher> :min <n> [:max <n>] [:as <sym>] [:greedy])"
  ;; Collect matcher and all remaining args
  (mzsubseq [(mzone (mvar 'child (mpred matcher-type)))
             (mzcollect (constantly true) 'rest 0)]
            nil)
  (fn [vars]
    (let [child (get vars 'child)
          rest-args (get vars 'rest)
          kw-args (parse-keyword-args rest-args)
          min-len (or (:min kw-args) 0)
          max-len (:max kw-args)
          sym (:as kw-args)
          greedy? (contains? kw-args :greedy)]
      (mzrepeat child min-len {:max-len max-len :greedy? greedy? :sym sym}))))

(defmatcher :seq "(? :seq [<matchers>...] [:min <n>] [:max <n>] [:as <sym>] [:greedy])"
  ;; Collect children vector and all remaining args
  (mzsubseq [(mzone (mvar 'children (mpred #(every? matcher-type %))))
             (mzcollect (constantly true) 'rest 0)]
            nil)
  (fn [vars]
    (let [children (get vars 'children)
          rest-args (get vars 'rest)
          kw-args (parse-keyword-args rest-args)
          min-len (:min kw-args)
          max-len (:max kw-args)
          sym (:as kw-args)
          greedy? (contains? kw-args :greedy)
          wrap-sym (fn [m] (if sym (mvar sym m) m))]
      (if min-len
        ;; Repeat the sequence pattern min to max times
        (let [sub-matchers (mapv (fn [c] (let [mt (matcher-type c)]
                                           (cond-> c (not= mt :subseq) mzone)))
                                 children)
              effective-max (or max-len min-len)
              rep-range (if greedy?
                          (range effective-max (dec min-len) -1)
                          (range min-len (inc effective-max)))
              alternatives (map (fn [n]
                                  (mseq (vec (concat (apply concat (repeat n sub-matchers))
                                                     [mterm]))))
                                rep-range)]
          (wrap-sym (mor alternatives)))
        ;; Basic case - just match sequence once
        (wrap-sym (mseq children))))))

(defmatcher :var "(? :var <sym> <matcher>)"
  (mzsubseq [(mzone (mvar 'sym (mpred symbol?)))
             (mzone (mvar 'child (mpred matcher-type)))]
            nil)
  (mvar ?sym ?child))

(defmatcher :named-var "(? :named-var <sym-or-nil> <flags-set>)"
  (mzsubseq [(mzone (mvar 'sym identity))
             (mzone (mvar 'flags (mpred set?)))]
            nil)
  (fn [vars]
    (let [sym (get vars 'sym)
          flags (get vars 'flags)]
      (cond
        (flags :one-or-more)  (repeat-matcher sym 1 (flags :greedy))
        (flags :zero-or-more) (repeat-matcher sym 0 (flags :greedy))
        :else (cond->> wildcard
                sym (mvar sym)
                (flags :optional) mzoption)))))

(defmatcher :or "(? :or <matcher>...)"
  (mzcollect matcher-type 'alternatives)
  (mor ?alternatives))

(defmatcher :not "(? :not <matcher>)"
  (mzone (mvar 'child (mpred matcher-type)))
  (fn [vars]
    (let [child (get vars 'child)]
      (matcher :not
               (fn [mr]
                 (let [result (child mr)]
                   (if (failure? result)
                     mr  ; child failed, so :not succeeds
                     (fail "negation failed: child matcher succeeded" :not (:val mr)))))))))

(defmatcher :-> "(? :-> <matcher>...)"
  (mzcollect matcher-type 'steps)
  (mchain ?steps))

(defmatcher :match-case "(? :match-case [<key> <matcher>...] [<sym>])"
  (mzsubseq [(mzone (mvar 'kv-matchers
                          (mpred #(and (vector? %)
                                       (even? (count %))
                                       (every? matcher-type (take-nth 2 (rest %)))))))
             (mzoption (mvar 'sym (mpred symbol?)))]
            nil)
  (mnor ?kv-matchers ?sym))

(defmatcher :filter "(? :filter <pred> [<sym>])"
  (mzsubseq [(mzone (mvar 'pred (mpred ifn?)))
             (mzoption (mvar 'sym (mpred symbol?)))]
            nil)
  (mzfilter ?pred {:sym ?sym}))

(defmatcher :first "(? :first <pred> [<sym>])"
  (mzsubseq [(mzone (mvar 'pred (mpred ifn?)))
             (mzoption (mvar 'sym (mpred symbol?)))]
            nil)
  (mzfirst ?pred {:sym ?sym}))

(defmatcher :sub "(? :sub <fn> [<matcher>])"
  (mzsubseq [(mzone (mvar 'f (mpred ifn?)))
             (mzoption (mvar 'child (mpred matcher-type)))]
            nil)
  (fn [vars]
    (let [f (get vars 'f)
          child (or (get vars 'child) wildcard)]
      (mchain [child (msub f)]))))

(defmatcher :update "(? :update <fn>)"
  (mzone (mvar 'f (mpred ifn?)))
  (fn [vars]
    (let [raw-f (get vars 'f)
          ;; Resolve symbol to function if needed (CLJ only, CLJS uses raw value)
          f #?(:clj (if (symbol? raw-f)
                      (if-let [v (resolve raw-f)] @v raw-f)
                      raw-f)
               :cljs raw-f)]
      (matcher :update
               (fn [mr]
                 (assoc mr :val (f (:val mr))))))))

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; :update matcher - applies function to value
  ;;-------------------------------------------------------------------
  ;; basic update: applies function to value
  ((ptn->matcher '(? :update inc) core-rules) (vmr 5))
  ;=>> {:val 6 :vars {}}

  ;; compose with :var to bind result
  ((ptn->matcher '(? :var n (? :update inc)) core-rules) (vmr 5))
  ;=>> {:val 6 :vars {'n 6}}
  )

(defmatcher :regex "(? :regex <pattern> [<sym>])"
  (mzsubseq [(mzone (mvar 'pattern (mpred regex?)))
             (mzoption (mvar 'sym (mpred symbol?)))]
            nil)
  (fn [vars]
    (let [pattern (get vars 'pattern)
          sym (get vars 'sym)]
      (matcher :regex
               (fn [mr]
                 (let [v (:val mr)]
                   (if-not (string? v)
                     (fail (str "expected string for regex match, got " (type v)) :regex v)
                     (if-let [result (re-matches pattern v)]
                       ;; re-matches returns: string (no groups), or vector [full-match & groups]
                       (let [groups (if (string? result) [result] result)]
                         (cond-> (assoc mr :val groups)
                           sym (-bind sym groups)))
                       (fail (str "regex " pattern " did not match: " (pr-str v)) :regex v)))))))))

(def ^:private matcher-specs
  "Unified specifications for each matcher type, populated by defmatcher."
  @matcher-specs*)

(defn- get-spec
  "Get spec for matcher type, throws if unknown with typo suggestions"
  [t]
  (or (get matcher-specs t)
      (let [valid-types (keys matcher-specs)
            suggestion (suggest-matcher-type t valid-types)]
        (throw (ex-info (str "Unknown matcher type: " t
                             (when suggestion (str ". Did you mean :" (name suggestion) "?"))
                             "\nValid types: " (sort valid-types))
                        {:type t :valid-types valid-types :suggestion suggestion})))))

(defn make-matcher
  "Construct a matcher from parsed vars using the spec's :make function"
  [{:keys [vars]}]
  ((:make (get-spec (get vars 'type))) vars))

(defn- check-matcher-args
  "Check matcher args and throw descriptive error on failure"
  [zmr]
  (let [t (get-in zmr [:vars 'type])
        spec (get-spec t)
        result ((:parse spec) zmr)]
    (if (failure? result)
      (throw (ex-info (str "Invalid arguments for (? " t " ...)\n"
                           "  Expected: " (:args spec) "\n"
                           "  Error: " (:reason result))
                      {:matcher-type t
                       :expected (:args spec)
                       :failure result}))
      result)))

(def core-rule
  "Rule for processing (? :type args...) forms"
  (mchain [(mseq [(mzone (mval '?))
                  (mzone (mvar 'type (mpred keyword?)))
                  check-matcher-args])
           (mf make-matcher)]))

(defn rule-of
  "Apply a rule to a pattern, returning the transformed value or nil"
  [rule ptn]
  (some-> ptn vmr rule :val))

;;## List-var pattern parsing
;; These functions parse list-form named variables like (?x even? [2 4] !)

(defn- var-symbol?
  "Check if x is a symbol starting with ? (before postwalk transforms it)"
  [x]
  (and (symbol? x)
       (let [n (name x)]
         (and (>= (count n) 2)
              (= \? (first n))))))

(defn- parse-var-name
  "Extract variable name from ?name symbol"
  [sym]
  (let [n (name sym)]
    (when (and (>= (count n) 2) (= \? (first n)))
      (let [base (clojure.core/subs n 1)]
        (when-not (= "_" base)
          (symbol base))))))

(defn- parse-list-var
  "Parse a list-form named variable like (?x even? [2 4] !) or (?x :update inc).
   Returns nil if not a valid list-var, or a map with:
   - :sym - the variable name (symbol or nil for wildcard)
   - :pred - predicate function (or nil)
   - :update-fn - update function (or nil) for transformations
   - :min-len - minimum length (nil for single value)
   - :max-len - maximum length (0 means unbounded)
   - :greedy? - true if greedy matching"
  [x]
  (when (and (list? x) (seq x))
    (let [[head & args] x]
      ;; Check if head is a raw ?name symbol (before postwalk transforms it)
      (when (var-symbol? head)
        (let [sym (parse-var-name head)]
          (loop [args args
                 result {:sym sym :pred nil :update-fn nil :min-len nil :max-len nil :greedy? false}]
            (if (empty? args)
              result
              (let [[arg & rest-args] args]
                (cond
                  ;; Greedy flag
                  (= '! arg)
                  (recur rest-args (assoc result :greedy? true))

                  ;; :update keyword followed by function
                  (= :update arg)
                  (if-let [[fn-arg & more-args] rest-args]
                    (let [update-fn (cond
                                      (symbol? fn-arg)
                                      #?(:clj (some-> (resolve fn-arg) deref)
                                         :cljs fn-arg)
                                      (ifn? fn-arg) fn-arg
                                      :else nil)]
                      (if update-fn
                        (recur more-args (assoc result :update-fn update-fn))
                        nil))  ;; Invalid update function
                    nil)  ;; :update without function

                  ;; Length vector [min max]
                  (and (vector? arg) (= 2 (count arg))
                       (every? #(and (number? %) (not (neg? %))) arg))
                  (let [[min-len max-len] arg]
                    (recur rest-args (assoc result :min-len min-len :max-len max-len)))

                  ;; Predicate symbol - resolve it first (before ifn? check)
                  (symbol? arg)
                  (if-let [resolved (resolve arg)]
                    (recur rest-args (assoc result :pred @resolved))
                    nil)  ;; Symbol doesn't resolve - invalid

                  ;; Predicate: function, map (key lookup), or set (membership test)
                  (or (fn? arg) (map? arg) (set? arg))
                  (recur rest-args (assoc result :pred arg))

                  ;; Invalid - not a list-var
                  :else nil)))))))))

(declare regex-matcher)

(defn- scalar-element
  "Convert a scalar value to a matcher.
   Functions and sets become predicates (sets test membership)."
  [x]
  (cond
    (matcher-type x) x
    (fn? x) (mpred x)
    (set? x) (mpred x)
    (regex? x) (regex-matcher x)
    :else (mval x)))

(defn ptn->matcher
  "Compile a pattern to a matcher function.
   Optionally takes custom rules that are tried before core-rules."
  ([ptn]
   (ptn->matcher ptn [core-rule]))
  ([ptn rules]
   (when-let [mch (mor rules)]
     ;; Use prewalk to handle list-var and named-var patterns before children are transformed,
     ;; then postwalk for everything else
     (let [prewalk-fn (fn [x]
                        (cond
                          ;; Regex patterns become regex matchers
                          (regex? x)
                          (regex-matcher x)
                          ;; Check list-var pattern first (before children transform)
                          (parse-list-var x)
                          (let [{:keys [sym pred update-fn min-len max-len greedy?]} (parse-list-var x)
                                base-child (if pred (mpred pred) wildcard)
                                ;; Apply update function if present
                                child (if update-fn
                                        (mchain [base-child (msub update-fn)])
                                        base-child)]
                            (if min-len
                              ;; max-len of 0 means unbounded (nil)
                              (let [effective-max (when-not (zero? max-len) max-len)]
                                (mzrepeat child min-len {:max-len effective-max :greedy? greedy? :sym sym}))
                              (if sym (mvar sym child) child)))
                          ;; Named-var symbols like ?x, ?x+, ?x* -> (? :named-var sym flags)
                          (named-var->form x)
                          (named-var->form x)
                          ;; Otherwise pass through
                          :else x))
           postwalk-fn (fn [x] (or (rule-of mch x) x))]
       (->> ptn
            (walk/prewalk prewalk-fn)
            (walk/postwalk postwalk-fn))))))

(defn- wrap-element
  "Wrap an element for use in a sequence matcher"
  [x]
  (let [mt (matcher-type x)]
    (cond-> x (not mt) scalar-element (not= mt :subseq) mzone)))

(defn- seq-matcher
  "Create a sequence matcher from elements"
  [x]
  (-> (map wrap-element x) (concat [mterm]) mseq))

(def vector-rule
  "Rule for matching vectors as sequences"
  (mchain [(mpred #(and (vector? %) (not (map-entry? %))))
           (msub seq-matcher)]))

(defn- map-matcher
  "Create a map matcher from key-value pairs"
  [x]
  (-> (map (fn [[k v]] [k (scalar-element v)]) x) mmap))

(def map-rule
  "Rule for matching maps"
  (mchain [(mpred map?)
           (msub map-matcher)]))

(defn- regex-matcher
  "Create a regex matcher from a pattern"
  [pattern]
  (matcher :regex
           (fn [mr]
             (let [v (:val mr)]
               (if-not (string? v)
                 (fail (str "expected string for regex match, got " (type v)) :regex v)
                 (if-let [result (re-matches pattern v)]
                   (let [groups (if (string? result) [result] result)]
                     (assoc mr :val groups))
                   (fail (str "regex " pattern " did not match: " (pr-str v)) :regex v)))))))

(def regex-rule
  "Rule for matching regex patterns against strings"
  (mchain [(mpred regex?)
           (msub regex-matcher)]))

(defn- repeat-matcher
  "Create a repeating matcher for + and * quantifiers.
   Default is lazy (match minimum), use greedy? true to match maximum."
  [sym min-len greedy?]
  ;; Use mzrepeat with nil max-len for unbounded matching (uses actual sequence length)
  (mzrepeat wildcard min-len {:max-len nil :greedy? greedy? :sym sym}))

(def core-rules
  "Default rules for pattern compilation: vector, map, regex, and core forms"
  [vector-rule map-rule regex-rule core-rule])

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; pred - predicate matching
  ;;-------------------------------------------------------------------
  ;; basic predicate
  ((rule-of core-rule (list '? :pred odd?)) (vmr 3)) ;=>> {:val 3}
  ;; predicate with args (f args... val) success case
  ((rule-of core-rule (list '? :pred < 5)) (vmr 10)) ;=>> {:val 10}
  ;; predicate with args failure case
  ((rule-of core-rule (list '? :pred < 5)) (vmr 3)) ;=>> failure?
  ;; predicate with $var reference to bound variable
  ((ptn->matcher (list '? :seq [(list '? :one (mvar 'a wildcard))
                                (list '? :one (list '? :pred < '$a))])) (vmr [2 5])) ;=>>
  {:val [2 5] :vars '{a 2}}
  ;; set as predicate - membership test (value in set)
  ((rule-of core-rule (list '? :pred #{:a :b :c})) (vmr :a)) ;=>> {:val :a}
  ;; set as predicate - failure case (value not in set)
  ((rule-of core-rule (list '? :pred #{:a :b :c})) (vmr :d)) ;=>> failure?
  ;; map as predicate - key lookup returns truthy
  ((rule-of core-rule (list '? :pred {:a 1 :b 2})) (vmr :a)) ;=>> {:val :a}
  ;; map as predicate - key lookup returns falsy (nil)
  ((rule-of core-rule (list '? :pred {:a 1 :b 2})) (vmr :c)) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; val - exact value matching
  ;;-------------------------------------------------------------------
  ((rule-of core-rule '(? :val 5)) (vmr 5)) ;=>> {:val 5}

  ;;-------------------------------------------------------------------
  ;; or - first successful alternative wins
  ;;-------------------------------------------------------------------
  ;; first alternative matches
  ((rule-of core-rule (list '? :or (mpred even?) (mval -1))) (vmr 4)) ;=>> {:val 4}
  ;; second alternative matches
  ((rule-of core-rule (list '? :or (mpred even?) (mval -1))) (vmr -1)) ;=>> {:val -1}
  ;; no alternative matches
  ((rule-of core-rule (list '? :or (mpred even?) (mval -1))) (vmr 3)) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; -> - sequential matcher chain
  ;;-------------------------------------------------------------------
  ;; chain succeeds when all steps pass
  ((rule-of core-rule (list '? :-> (mpred even?) (mvar 'x wildcard))) (vmr 4)) ;=>>
  {:val 4 :vars '{x 4}}
  ;; chain fails when any step fails
  ((rule-of core-rule (list '? :-> (mpred even?) (mpred neg?))) (vmr 4)) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; match-case - case matching with key binding
  ;;-------------------------------------------------------------------
  ;; binds matched key to symbol
  ((rule-of core-rule (list '? :match-case [:even (mpred even?) :neg-one (mval -1)] 'which)) (vmr 4)) ;=>>
  {:val 4 :vars '{which :even}}
  ;; falls through to second case
  ((rule-of core-rule (list '? :match-case [:even (mpred even?) :neg-one (mval -1)] 'which)) (vmr -1)) ;=>>
  {:val -1 :vars '{which :neg-one}}
  ;; fails when no case matches
  ((rule-of core-rule (list '? :match-case [:even (mpred even?) :neg-one (mval -1)] 'which)) (vmr 3)) ;=>>
  failure?

  ;;-------------------------------------------------------------------
  ;; filter - collect matching elements from sequence
  ;;-------------------------------------------------------------------
  ((ptn->matcher (list '? :seq [(list '? :filter even? 'evens)])) (vmr [1 2 3 4 5 6])) ;=>>
  {:vars '{evens [2 4 6]}}
  ;; filter with set predicate - collects elements in set
  ((ptn->matcher (list '? :seq [(list '? :filter #{:a :b} 'matched)])) (vmr [:a :c :b :d])) ;=>>
  {:vars '{matched [:a :b]}}

  ;;-------------------------------------------------------------------
  ;; first - find first matching element
  ;;-------------------------------------------------------------------
  ;; succeeds - returns first match
  ((ptn->matcher (list '? :seq [(list '? :first even? 'first-even)])) (vmr [1 3 5 6 7 8])) ;=>>
  {:val 6 :vars '{first-even 6}}
  ;; fails when no element matches
  ((ptn->matcher (list '? :seq [(list '? :first neg?)])) (vmr [1 2 3])) ;=>> failure?
  ;; first with set predicate - finds first element in set
  ((ptn->matcher (list '? :seq [(list '? :first #{:x :y} 'found)])) (vmr [:a :b :x :y])) ;=>>
  {:val :x :vars '{found :x}}

  ;;-------------------------------------------------------------------
  ;; sub - value transformation
  ;;-------------------------------------------------------------------
  ;; with child matcher transforms after successful match
  ((rule-of core-rule (list '? :sub inc (mpred even?))) (vmr 8)) ;=>> {:val 9}
  ;; without child matcher transforms any value
  ((rule-of core-rule (list '? :sub inc)) (vmr 5)) ;=>> {:val 6}
  ;; fails when child matcher fails
  ((rule-of core-rule (list '? :sub inc (mpred even?))) (vmr 7)) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; not - negation matcher
  ;;-------------------------------------------------------------------
  ;; succeeds when child fails
  ((rule-of core-rule (list '? :not (mpred even?))) (vmr 3)) ;=>> {:val 3}
  ;; fails when child succeeds
  ((rule-of core-rule (list '? :not (mpred even?))) (vmr 4)) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; regex - regex pattern matching (strings only)
  ;;-------------------------------------------------------------------
  ;; basic match without groups - returns full match in vector
  ((rule-of core-rule (list '? :regex #"hello")) (vmr "hello")) ;=>> {:val ["hello"]}
  ;; match with capture groups - returns [full-match group1 group2 ...]
  ((rule-of core-rule (list '? :regex #"(\d+)-(\d+)")) (vmr "123-456")) ;=>> {:val ["123-456" "123" "456"]}
  ;; bind groups to symbol
  ((rule-of core-rule (list '? :regex #"(\w+)@(\w+)" 'groups)) (vmr "user@host")) ;=>>
  {:val ["user@host" "user" "host"] :vars '{groups ["user@host" "user" "host"]}}
  ;; fails on non-string input
  ((rule-of core-rule (list '? :regex #"\d+")) (vmr 123)) ;=>> failure?
  ;; fails when pattern doesn't match
  ((rule-of core-rule (list '? :regex #"^\d+$")) (vmr "abc")) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; regex-rule - automatic regex pattern conversion
  ;;-------------------------------------------------------------------
  ;; regex literal automatically becomes matcher
  ((ptn->matcher #"hello" core-rules) (vmr "hello")) ;=>> {:val ["hello"]}
  ;; with capture groups
  ((ptn->matcher #"(\d+)-(\d+)" core-rules) (vmr "12-34")) ;=>> {:val ["12-34" "12" "34"]}
  ;; regex in map pattern (transforms value to groups)
  ((ptn->matcher {:name #"(\w+) (\w+)"} core-rules) (vmr {:name "John Doe"})) ;=>> {:val {:name ["John Doe" "John" "Doe"]}}
  ;; regex in vector pattern with variable binding
  (query '[?first ?second] ["hello" "123"]) ;=>> '{first "hello" second "123"}
  ;; regex validates string in vector (use vector literal, not quote)
  (query ['?x #"\d+"] ["hello" "123"]) ;=>> '{x "hello"}

  ;;-------------------------------------------------------------------
  ;; repeat - repeated element matching
  ;;-------------------------------------------------------------------
  ;; lazy (default) matches minimum needed
  ((ptn->matcher (list '? :seq [(list '? :repeat (mpred even?) :min 2 :max 3 :as 'evens)])) (vmr [2 4])) ;=>>
  {:vars '{evens (2 4)}}
  ;; greedy matches maximum possible
  ((ptn->matcher (list '? :seq [(list '? :repeat (mpred even?) :min 2 :max 3 :as 'evens :greedy true)])) (vmr [2 4 6])) ;=>>
  {:vars '{evens (2 4 6)}}
  ;; fails when below minimum
  ((ptn->matcher (list '? :seq [(list '? :repeat (mpred even?) :min 2 :max 3)])) (vmr [2])) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; seq - sequence pattern with repetition
  ;;-------------------------------------------------------------------
  ;; min - repeat pattern exactly min times
  ((ptn->matcher (list '? :seq [(mval 1) (mval 2)] :min 2)) (vmr [1 2 1 2])) ;=>> {:val [1 2 1 2]}
  ;; fails when count < min
  ((ptn->matcher (list '? :seq [(mval 1) (mval 2)] :min 2)) (vmr [1 2])) ;=>> failure?
  ;; min and max - repeat between min and max times (lazy)
  ((ptn->matcher (list '? :seq [(mval 1)] :min 2 :max 3)) (vmr [1 1])) ;=>> {:val [1 1]}
  ;; as - bind matched sequence
  ((ptn->matcher (list '? :seq [(mval 1) (mval 2)] :min 1 :max 2 :as 'x)) (vmr [1 2])) ;=>> {:vars '{x [1 2]}}

  ;;-------------------------------------------------------------------
  ;; Complex patterns
  ;;-------------------------------------------------------------------
  ;; nested map pattern with variable unification
  ((ptn->matcher '(? :map {:a (? :var a (? :val 3)) :b (? :map {:c (? :var a (? :val 3))})})) (vmr {:a 3 :b {:c 3}})) ;=>>
  {:vars '{a 3}}
  ;; optional element in sequence
  ((ptn->matcher '(? :seq [(? :optional (? :val 1)) (? :one (? :val 10))])) (vmr [10])) ;=>> {:val [10]}
  ;; map-rule compiles map literals
  ((ptn->matcher '{:a (? :var a (? :val 5))} [map-rule core-rule]) (vmr {:a 5})) ;=>> {:val {:a 5}}
  )

;;=============================================================================
;; SECTION 5: CompiledMatcher and Public API
;;=============================================================================

;;## Compiled Matcher Protocol
;;
;; IMatcher provides multiple ways to access match results from a compiled pattern.
;; CompiledMatcher implements both IFn (for raw results) and IMatcher (for convenience).

(defprotocol IMatcher
  "Protocol for compiled matchers providing multiple result access patterns."
  (-query [this data] "Returns vars map or nil on failure")
  (-match-result [this data] "Returns {:vars ...} on success or MatchFailure on failure")
  (-match! [this data] "Returns vars map or throws on failure"))

(defrecord CompiledMatcher [matcher-fn pattern]
  IMatcher
  (-query [this data]
    (let [result (matcher-fn (vmr data))]
      (when-not (failure? result)
        (:vars result))))

  (-match-result [this data]
    (let [result (matcher-fn (vmr data))]
      (if (failure? result)
        result
        {:vars (:vars result)})))

  (-match! [this data]
    (let [result (matcher-fn (vmr data))]
      (if (failure? result)
        (throw (ex-info (str "Match failed: " (:reason result))
                        {:failure result
                         :pattern pattern
                         :data data
                         :path (:path result)}))
        (:vars result))))

  #?@(:clj
      [clojure.lang.IFn
       (invoke [this data]
               (matcher-fn (vmr data)))]
      :cljs
      [IFn
       (-invoke [this data]
                (matcher-fn (vmr data)))]))

(defn compiled-matcher
  "Create a CompiledMatcher from a matcher function and original pattern"
  [matcher-fn pattern]
  (->CompiledMatcher matcher-fn pattern))

(defn compiled-matcher?
  "Returns true if x is a CompiledMatcher"
  [x]
  (instance? CompiledMatcher x))

;; Extend IMatcher to support raw patterns (compiles on each call - for REPL convenience)

(defn- raw-pattern->compiled
  "Compile a raw pattern (map, vector, list) into a CompiledMatcher"
  [ptn]
  (compiled-matcher (ptn->matcher ptn core-rules) ptn))

(def ^:private raw-pattern-imatcher-impl
  "Shared IMatcher implementation for raw patterns - compiles on each call"
  {:-query (fn [this data] (-query (raw-pattern->compiled this) data))
   :-match-result (fn [this data] (-match-result (raw-pattern->compiled this) data))
   :-match! (fn [this data] (-match! (raw-pattern->compiled this) data))})

#?(:clj
   (do
     (extend clojure.lang.IPersistentMap IMatcher raw-pattern-imatcher-impl)
     (extend clojure.lang.IPersistentVector IMatcher raw-pattern-imatcher-impl)
     (extend clojure.lang.IPersistentList IMatcher raw-pattern-imatcher-impl))
   :cljs
   (do
     (extend-type PersistentArrayMap IMatcher
                  (-query [this data] (-query (raw-pattern->compiled this) data))
                  (-match-result [this data] (-match-result (raw-pattern->compiled this) data))
                  (-match! [this data] (-match! (raw-pattern->compiled this) data)))
     (extend-type PersistentHashMap IMatcher
                  (-query [this data] (-query (raw-pattern->compiled this) data))
                  (-match-result [this data] (-match-result (raw-pattern->compiled this) data))
                  (-match! [this data] (-match! (raw-pattern->compiled this) data)))
     (extend-type PersistentVector IMatcher
                  (-query [this data] (-query (raw-pattern->compiled this) data))
                  (-match-result [this data] (-match-result (raw-pattern->compiled this) data))
                  (-match! [this data] (-match! (raw-pattern->compiled this) data)))
     (extend-type List IMatcher
                  (-query [this data] (-query (raw-pattern->compiled this) data))
                  (-match-result [this data] (-match-result (raw-pattern->compiled this) data))
                  (-match! [this data] (-match! (raw-pattern->compiled this) data)))))

;; Public wrapper functions
(defn query
  "Match pattern against data and return variable bindings, or nil if no match.
   Pattern can be a CompiledMatcher or a raw pattern (compiled on each call)."
  [ptn data]
  (-query ptn data))

(defn match-result
  "Match pattern against data and return {:vars ...} on success or MatchFailure on failure.
   Pattern can be a CompiledMatcher or a raw pattern (compiled on each call)."
  [ptn data]
  (-match-result ptn data))

(defn match!
  "Match pattern against data and return vars map, or throw on failure.
   Pattern can be a CompiledMatcher or a raw pattern (compiled on each call)."
  [ptn data]
  (-match! ptn data))

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; Pattern DSL - symbol form variables
  ;;-------------------------------------------------------------------
  ;; named-var internal form
  ((rule-of core-rule '(? :named-var x #{})) (vmr 5)) ;=>> {:vars '{x 5}}
  ;; nested map with variable unification (?a appears twice, must match)
  (query '{:a ?a :b {:c ?a}} {:a 3 :b {:c 3}}) ;=>> '{a 3}
  ;; wildcard ?_ matches but doesn't bind, optional ?a? matches if present
  (query '[3 ?_ ?a?] [3 0 4]) ;=>> '{a 4}

  ;;-------------------------------------------------------------------
  ;; ?x+ (one or more, lazy)
  ;;-------------------------------------------------------------------
  ;; matches remaining elements after fixed prefix
  (query '[1 ?rest+] [1 2 3 4]) ;=>> '{rest [2 3 4]}
  ;; fails on empty remainder
  (query '[?rest+] []) ;=> nil

  ;;-------------------------------------------------------------------
  ;; ?x* (zero or more, lazy)
  ;;-------------------------------------------------------------------
  ;; matches remaining elements
  (query '[1 ?rest*] [1 2 3]) ;=>> '{rest [2 3]}
  ;; matches empty remainder
  (query '[1 ?rest*] [1]) ;=>> '{rest []}

  ;;-------------------------------------------------------------------
  ;; Lazy vs greedy quantifiers
  ;;-------------------------------------------------------------------
  ;; lazy (default) first quantifier takes minimum
  (query '[?a* ?b*] [1 2 3]) ;=>> '{a () b (1 2 3)}
  ;; greedy (!) first quantifier takes maximum
  (query '[?a*! ?b*] [1 2 3]) ;=>> '{a (1 2 3) b ()}
  ;; with minimum constraints
  (query '[?a+ ?b+] [1 2 3]) ;=>> '{a (1) b (2 3)}
  (query '[?a+! ?b+] [1 2 3]) ;=>> '{a (1 2) b (3)}

  ;;-------------------------------------------------------------------
  ;; query/match-result failure handling
  ;;-------------------------------------------------------------------
  ;; query returns nil on mismatch
  (query '{:a 5} {:a 3}) ;=> nil
  ;; match-result returns MatchFailure with path
  (match-result '{:a 5} {:a 3}) ;=>> {:reason #"value mismatch" :path [:a] :depth 1}
  ;; nested failure tracks full path
  (match-result '{:a {:b 10}} {:a {:b 20}}) ;=>> {:path [:a :b] :depth 2}

  ;;-------------------------------------------------------------------
  ;; List-form named variables (?x pred? [min max]? !?)
  ;;-------------------------------------------------------------------
  ;; with predicate succeeds or fails based on predicate
  (query '[(?x even?)] [4]) ;=>> '{x 4}
  (query '[(?x even?)] [3]) ;=> nil
  ;; with set as predicate - membership test
  (query '[(?x #{:a :b :c})] [:b]) ;=>> '{x :b}
  (query '[(?x #{:a :b :c})] [:d]) ;=> nil
  ;; with map as predicate - key lookup
  (query '[(?x {:valid true :invalid false})] [:valid]) ;=>> '{x :valid}
  (query '[(?x {:valid true :invalid false})] [:unknown]) ;=> nil
  ;; with length range [min max]
  (query '[(?x [2 4])] [1 2 3]) ;=>> '{x (1 2 3)}
  (query '[(?x [2 4])] [1]) ;=> nil
  ;; with predicate and length
  (query '[(?x even? [2 3])] [2 4 6]) ;=>> '{x (2 4 6)}
  ;; unbounded (max=0 means match to end)
  (query '[(?x [1 0])] [1 2 3]) ;=>> '{x (1 2 3)}
  ;; lazy vs greedy
  (query '[(?x [0 5]) ?y*] [1 2 3]) ;=>> '{x () y (1 2 3)}
  (query '[(?x [0 5] !) ?y*] [1 2 3]) ;=>> '{x (1 2 3) y ()}

  ;;-------------------------------------------------------------------
  ;; CompiledMatcher protocol
  ;;-------------------------------------------------------------------
  (def cm (compiled-matcher (ptn->matcher '{:a ?a} core-rules) '{:a ?a}))
  ;; callable as function - returns ValMatchResult
  (cm {:a 42}) ;=>> {:val {:a 42} :vars '{a 42}}
  ;; query - returns vars or nil
  (query cm {:a 42}) ;=>> '{a 42}
  (query cm "not a map") ;=> nil
  ;; match-result - returns {:vars} or MatchFailure
  (match-result cm {:a 42}) ;=>> {:vars '{a 42}}
  (match-result cm "not a map") ;=>> failure?
  ;; match! - returns vars (throws on failure)
  (match! cm {:a 42}) ;=>> '{a 42}

  ;;-------------------------------------------------------------------
  ;; Sets as predicates in patterns (scalar-element)
  ;;-------------------------------------------------------------------
  ;; set in map pattern - tests membership
  (query {:status #{:active :pending}} {:status :active}) ;=>> {}
  (query {:status #{:active :pending}} {:status :inactive}) ;=> nil
  ;; set in vector pattern - tests membership
  (query [#{:a :b} '?x] [:a 42]) ;=>> '{x 42}
  (query [#{:a :b} '?x] [:c 42]) ;=> nil
  ;; set with variable binding in map
  (query '{:type ?t :status #{:ok :warn}} {:type "test" :status :ok}) ;=>> '{t "test"}
  )
