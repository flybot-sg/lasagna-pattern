(ns sg.flybot.pullable.core
  "Core pattern matching engine.

   This namespace provides the pattern matching implementation:
   - Match result types (ValMatchResult, MatchFailure)
   - Matcher primitives (mpred, mval, mmap, mseq, mor, etc.)
   - Pattern DSL and compilation (ptn->matcher, ptn->core, core->matcher, defmatcher)"
  (:require
   [clojure.pprint :refer [cl-format]]
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
;; | -rule  | pattern compilation rules  | core-rule                                      |

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
  (-bind (vmr 3) 'a 4) ;=>>
  {:vars {'a 4}})

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
  (matcher :pred
           (fn [{:keys [val] :as mr}]
             (if (pred val)
               mr
               (fail (cl-format nil "Predicate fail for value ~A" val) :val val)))))

(defn mval
  "a matcher that succeeds when mr's value equals v"
  [v]
  (mpred #(= % v)))

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
  ((mf (fn [{:keys [vars val]}] (+ val (vars 'a)))) (vmr 3 {'a 2})) ;=>> 
  {:val 5})

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

(defn mcase
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
                     (cond-> result sym (-bind sym k))))
                 (or best-fail (fail "no matchers matched" :nor (:val mr))))))))

^:rct/test
(comment
  ;; mor returns first successful match (short-circuits)
  (map (mor [(mpred neg?) (mpred even?)]) [(vmr -1) (vmr 2) (vmr 3)]) ;=>>
  [{} {} failure?]
  ;; mnor tries matchers in order, binds key of first success to symbol
  ((mcase [0 (mpred even?) 1 (mpred odd?)] 'b) (vmr 3)) ;=>> 
  {:vars {'b 1}})

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
  (mm (vmr {:a 4 :b 3})) ;=>>
  failure?)

;;## Sequence matchers
;;
;;The key function is `mseq` which contains sub-matchers, these sub-matchers
;;use clojure.zip's zipper as the `:val` of the mr, the convention is `zmr`,
;;The name convention for this kind of sub-matchers starting from `mz`, rather than `m`

(defn zipper?
  "Check if x is a valid clojure.zip zipper structure.
   Zippers are vectors of [node meta] where meta is nil, :end, or a map."
  [x]
  (and (vector? x)
       (= 2 (count x))
       (let [meta (second x)]
         (or (nil? meta)
             (= :end meta)
             (map? meta)))))

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
               (cond
                 (not (zipper? zip))
                 (fail "expected zipper context (use [pattern*] for sequence matching)" :subseq zip)

                 (zip/end? zip)
                 (fail "unexpected end of sequence" :subseq nil)

                 :else
                 (let [node (zip/node zip)
                       result (child (assoc zmr :val node))]
                   (if (failure? result)
                     result
                     (-> zmr
                         (vapply (fn [z] (let [nv (:val result)] (-> (if (= node nv) z (zip/replace z nv)) zip-advance))))
                         (merge-vars result)))))))))

^:rct/test
(comment
  ;; zipper? recognizes valid zipper structures
  (zipper? (-> (zip/vector-zip [1 2]) zip/down)) ;=> true
  (zipper? [nil :end]) ;=> true
  (zipper? [1 nil]) ;=> true
  ;; zipper? rejects non-zipper values
  (zipper? '(1 2 3)) ;=> false
  (zipper? [1 2 3]) ;=> false
  (zipper? "hello") ;=> false

  ;; mzone matches single element in zipper sequence
  ((mzone (mvar 'a (mval 3))) (vmr (-> (zip/vector-zip [3 4]) zip/down))) ;=>> {:vars {'a 3}}
  ;; mzone fails at end of sequence (no element to match)
  ((mzone (mvar 'a (mval 3))) (vmr [nil :end])) ;=>> failure?
  ;; mzone fails gracefully with non-zipper input (instead of throwing exception)
  ((mzone wildcard) (vmr '("hello" "world"))) ;=>> 
  failure?)

(defn- optional [x] (some-> x meta ::optional))

(defn- expand-optionals
  "Expand optional sub-matchers into proper alternatives.
   Each optional is expanded with its successors as nxt, chaining optionals
   together (required for lazy/greedy semantics)."
  [sub-matchers]
  (reduce (fn [acc head]
            (if-let [opt-fn (optional head)]
              [(opt-fn head acc)]
              (cons head acc)))
          []
          (reverse sub-matchers)))

(defn mzsubseq
  "a sub-matcher can contains `sub-matchers`, bind the result to `sym` if not nil,
   if a `len` is specified, bind the part of `len`"
  ([sub-matchers sym]
   (mzsubseq sub-matchers sym nil))
  ([sub-matchers sym len]
   (let [children (expand-optionals sub-matchers)]
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

(defn parse-matching-var
  "Parse a quantified variable symbol like x?, x+, x*, x+!, x*!.
   Returns map with :sym, :quantifier, :greedy? or nil if not a matching-var."
  [v]
  (when (symbol? v)
    (when-let [[_ base suffix greedy] (re-matches #"(.+)([\?\+\*])(\!?)" (name v))]
      {:sym (when (not= "_" base) (symbol base))
       :quantifier (case suffix "?" :optional "+" :one-or-more "*" :zero-or-more)
       :greedy? (= "!" greedy)})))

(defn- quantifier->min-max
  "Convert a quantifier keyword to [min max] where max=0 means unbounded.
   Returns nil for :optional (handled differently)."
  [quantifier]
  (case quantifier
    :zero-or-more [0 0]
    :one-or-more [1 0]
    nil))

^:rct/test
(comment
  ;; parse-matching-var parses optional x?
  (parse-matching-var 'x?) ;=>> {:sym 'x :quantifier :optional :greedy? false}
  ;; parse-matching-var parses one-or-more x+ (lazy)
  (parse-matching-var 'x+) ;=>> {:sym 'x :quantifier :one-or-more :greedy? false}
  ;; parse-matching-var parses zero-or-more x* (lazy)
  (parse-matching-var 'x*) ;=>> {:sym 'x :quantifier :zero-or-more :greedy? false}
  ;; parse-matching-var parses greedy one-or-more x+!
  (parse-matching-var 'x+!) ;=>> {:sym 'x :quantifier :one-or-more :greedy? true}
  ;; parse-matching-var parses greedy zero-or-more x*!
  (parse-matching-var 'x*!) ;=>> {:sym 'x :quantifier :zero-or-more :greedy? true}
  ;; parse-matching-var wildcard _* returns nil symbol
  (parse-matching-var '_*) ;=>> {:sym nil :quantifier :zero-or-more :greedy? false}
  ;; parse-matching-var returns nil for plain symbols without quantifier
  (parse-matching-var 'x)) ;=> nil)

;;=============================================================================
;; SECTION 3: Pattern DSL and Compilation
;;=============================================================================

(declare repeat-matcher)

(def ^:private matcher-specs* (atom {}))

(defmacro defmatcher
  "Define a matcher type specification.
   type - keyword like :pred, :val, etc.
   args - human-readable argument format string
   parse - matcher that validates and extracts arguments
   make - a (fn [vars] ...) that creates the matcher from parsed vars"
  [type args parse make]
  `(swap! matcher-specs* assoc ~type
          {:args ~args
           :parse ~parse
           :make ~make}))

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

(defmatcher :pred "(? :pred <fn>)"
  (mzone (mvar 'f identity))
  (fn [vars] (mpred (get vars 'f))))

(defmatcher :val "(? :val <value>)"
  (mzone (mvar 'val identity))
  (fn [vars] (mval (get vars 'val))))

(defmatcher :any "(? :any)"
  mterm  ;; no args expected
  (fn [_vars] wildcard))

(defmatcher :map "(? :map <map>)"
  (mzone (mvar 'map (mpred map?)))
  (fn [vars] (mmap (get vars 'map))))

(defmatcher :one "(? :one <matcher>)"
  (mzone (mvar 'child (mpred matcher-type)))
  (fn [vars] (mzone (get vars 'child))))

(defmatcher :optional "(? :optional <matcher>)"
  (mzone (mvar 'child (mpred matcher-type)))
  (fn [vars] (mzoption (get vars 'child))))

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
        ;; Basic case - add mterm for termination
        (wrap-sym (mseq (conj (vec children) mterm)))))))

(defmatcher :var "(? :var <sym> <matcher>)"
  (mzsubseq [(mzone (mvar 'sym (mpred symbol?)))
             (mzone (mvar 'child (mpred matcher-type)))]
            nil)
  (fn [vars] (mvar (get vars 'sym) (get vars 'child))))

(defmatcher :matching-var "(? :matching-var <sym-or-nil> <flags-set>)"
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
  (fn [vars] (mor (get vars 'alternatives))))

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
  (fn [vars] (mchain (get vars 'steps))))

(defmatcher :match-case "(? :match-case <key> <matcher>... [<sym>])"
  ;; Collect all remaining args - parsed in builder to handle optional trailing sym
  (mzcollect (constantly true) 'items 2)
  (fn [vars]
    (let [items (get vars 'items)
          n (count items)
          ;; If odd count, last item is sym for binding the matched key
          [kv-items sym] (if (odd? n)
                           [(butlast items) (last items)]
                           [items nil])
          ;; Validate structure: even count, matchers at odd indices
          _ (when-not (and (even? (count kv-items))
                           (every? matcher-type (take-nth 2 (rest kv-items))))
              (throw (ex-info (str "Invalid :match-case arguments. Expected: "
                                   "(? :match-case <key> <matcher>... [<sym>])")
                              {:items items})))]
      (mcase (vec kv-items) sym))))

(defmatcher :filter "(? :filter <pred> [<sym>])"
  (mzsubseq [(mzone (mvar 'pred (mpred ifn?)))
             (mzoption (mvar 'sym (mpred symbol?)))]
            nil)
  (fn [vars] (mzfilter (get vars 'pred) {:sym (get vars 'sym)})))

(defmatcher :first "(? :first <pred> [<sym>])"
  (mzsubseq [(mzone (mvar 'pred (mpred ifn?)))
             (mzoption (mvar 'sym (mpred symbol?)))]
            nil)
  (fn [vars] (mzfirst (get vars 'pred) {:sym (get vars 'sym)})))

(defmatcher :sub "(? :sub [<matcher>] <fn>)"
  (mzsubseq [(mzoption (mvar 'child (mpred matcher-type)))
             (mzone (mvar 'f (mpred ifn?)))]
            nil)
  (fn [vars]
    (let [f (get vars 'f)
          child (or (get vars 'child) wildcard)]
      (mchain [child (msub f)]))))

(defmatcher :update "(? :update <fn>)"
  (mzone (mvar 'f (mpred ifn?)))
  (fn [vars]
    (let [f (get vars 'f)]
      (matcher :update
               (fn [mr]
                 (assoc mr :val (f (:val mr))))))))

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; :update matcher - applies function to value
  ;; Note: function args require `list` to pass actual fn values
  ;;-------------------------------------------------------------------
  ;; basic update: applies function to value
  ((ptn->matcher (list '? :update inc)) (vmr 5))
  ;=>> {:val 6 :vars {}}

  ;; compose with :var to bind result
  ((ptn->matcher (list '? :var 'n (list '? :update inc))) (vmr 5)))
  ;=>> {:val 6 :vars {'n 6}}

(defmatcher :regex "(? :regex <pattern>)"
  (mzone (mvar 'pattern (mpred regex?)))
  (fn [vars]
    (let [pattern (get vars 'pattern)]
      (matcher :regex
               (fn [mr]
                 (let [v (:val mr)]
                   (if-not (string? v)
                     (fail (str "expected string for regex match, got " (type v)) :regex v)
                     (if-let [result (re-matches pattern v)]
                       ;; re-matches returns: string (no groups), or vector [full-match & groups]
                       (assoc mr :val (if (string? result) [result] result))
                       (fail (str "regex " pattern " did not match: " (pr-str v)) :regex v)))))))))

(def ^:private matcher-specs
  "Unified specifications for each matcher type, populated by defmatcher."
  @matcher-specs*)

(defn- get-spec
  "Get spec for matcher type, throws if unknown"
  [t]
  (or (get matcher-specs t)
      (let [valid-types (keys matcher-specs)]
        (throw (ex-info (str "Unknown matcher type: " t "\nValid types: " (sort valid-types))
                        {:type t :valid-types valid-types})))))

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
;; Parse list-form variables like (x pred? [min max]? :update fn? !)
;; Uses matchers for validation (dogfooding)

(def ^:private list-var-matcher
  "Matches list-var patterns with fixed order:
   (sym pred? [min max]? :update fn? !?)
   where sym is a symbol (not ?), pred is fn/map/set, [min max] is length range.
   Note: :update and fn must appear together (validated in parse-list-var)"
  (mseq [(mzone (mvar 'sym (mpred #(and (symbol? %) (not= '? %)))))
         (mzoption (mvar 'pred (mpred #(or (fn? %) (map? %) (set? %)))))
         (mzoption (mvar 'len (mpred #(and (vector? %) (= 2 (count %))
                                           (every? nat-int? %)))))
         (mzoption (mval :update))
         ;; update-fn must be fn (not symbol, since symbols are IFn but not intended here)
         (mzoption (mvar 'update-fn (mpred fn?)))
         ;; ! may be read as character \! or symbol '! depending on Clojure version/reader
         (mzoption (mvar 'greedy (mpred #(or (= \! %) (= '! %)))))
         mterm]))

(defn- parse-list-var
  "Parse a list-form variable like (x even? [2 4] :update inc !).
   Fixed order: (sym pred? [min max]? :update fn? !?)
   Also handles quantifier suffixes: (x* fn?) is equivalent to (x fn? [0 0]).
   Returns nil if not a valid list-var, or the parsed vars map."
  [x]
  (when (and (list? x) (seq x))
    (let [result (list-var-matcher (vmr (seq x)))]
      (when-not (failure? result)
        (let [{:syms [sym pred len update-fn greedy]} (:vars result)
              ;; Check if symbol has quantifier suffix (x*, x+, x?)
              quantified (when sym (parse-matching-var sym))
              ;; Extract base symbol and quantifier-derived min/max
              base-sym (if quantified (:sym quantified) sym)
              [quant-min quant-max] (quantifier->min-max (:quantifier quantified))
              quant-greedy? (:greedy? quantified)
              ;; Explicit [min max] overrides quantifier-derived values
              [explicit-min explicit-max] len
              final-min (or explicit-min quant-min)
              final-max (or explicit-max quant-max)]
          {:sym (when (and base-sym (not= '_ base-sym)) base-sym)
           :pred pred
           :update-fn update-fn
           :min-len final-min
           :max-len final-max
           :greedy? (or (some? greedy) quant-greedy?)})))))

(defn- double-quoted-symbol?
  "Check if x is a double-quoted symbol form: ''sym -> (quote (quote sym)).
   Returns the inner symbol if true, nil otherwise."
  [x]
  (when (and (seq? x)
             (= 2 (count x))
             (= 'quote (first x)))
    (let [inner (second x)]
      (when (and (seq? inner)
                 (= 2 (count inner))
                 (= 'quote (first inner))
                 (symbol? (second inner)))
        (second inner)))))

(defn- plain-var-symbol?
  "Check if x is a plain symbol that should become a variable binding.
   Returns the symbol name if true, nil otherwise.

   A symbol becomes a variable if it:
   - Is a symbol (not keyword, not other type)
   - Does not have a quantifier suffix (x*, x+, x?)
   - Is not ? (used in pattern forms)
   - Is not _ (reserved for possible future use)
   - Is not a special form (if, do, quote, etc.)"
  [x]
  (when (and (symbol? x)
             (not (parse-matching-var x))  ; not x*, x+, x? syntax
             (not= '? x)           ; not the pattern marker
             (not= '_ x)           ; not underscore
             (not (#{'if 'do 'let 'fn 'quote 'var 'def 'loop 'recur
                     'throw 'try 'catch 'finally 'new 'set!} x)))
    x))

(defn- core-pattern?
  "Check if x is a core (? :type ...) pattern"
  [x]
  (and (seq? x) (= '? (first x)) (keyword? (second x))))

(defn- subseq-type?
  "Check if pattern type is a subsequence matcher (not wrapped in :one)"
  [t]
  (#{:optional :repeat :filter :first} t))

(defn- wrap-for-seq
  "Wrap a core pattern for use in sequence context.
   Subsequence patterns (:optional, :repeat, :filter, :first) are not wrapped.
   Other patterns get wrapped in (? :one ...)"
  [ptn]
  (if (and (core-pattern? ptn) (subseq-type? (second ptn)))
    ptn
    (list '? :one ptn)))

(defn- matching-var->core
  "Convert a parsed matching-var to core pattern"
  [{:keys [sym quantifier greedy?]}]
  (case quantifier
    :optional
    (let [inner (if sym (list '? :var sym '(? :any)) '(? :any))]
      (list '? :optional inner))

    :one-or-more
    (cond-> (list '? :repeat '(? :any) :min 1)
      sym (concat [:as sym])
      greedy? (concat [:greedy])
      true (->> (apply list)))

    :zero-or-more
    (cond-> (list '? :repeat '(? :any) :min 0)
      sym (concat [:as sym])
      greedy? (concat [:greedy])
      true (->> (apply list)))))

(defn- list-var->core
  "Convert a parsed list-var to core pattern"
  [{:keys [sym pred update-fn min-len max-len greedy?]}]
  (let [base-ptn (if pred (list '? :pred pred) '(? :any))
        with-update (if update-fn
                      (list '? :-> base-ptn (list '? :update update-fn))
                      base-ptn)]
    (if min-len
      ;; Repeated element - use :repeat
      (cond-> (list '? :repeat with-update :min min-len)
        (and max-len (not (zero? max-len))) (concat [:max max-len])
        sym (concat [:as sym])
        greedy? (concat [:greedy])
        true (->> (apply list)))
      ;; Single element
      (if sym
        (list '? :var sym with-update)
        with-update))))

;;-------------------------------------------------------------------
;; Rewrite rules for ptn->core
;; Each rule takes (x rewrite) and returns rewritten pattern or nil
;;-------------------------------------------------------------------

(defn- rewrite-arg
  "Rewrite an argument in a core pattern.
   Some arg types should stay as-is (keywords, numbers, raw functions)."
  [rewrite arg]
  (cond
    (vector? arg) (mapv rewrite arg)
    (map? arg) (into {} (map (fn [[k v]] [k (rewrite v)])) arg)
    (or (keyword? arg) (number? arg)) arg
    (symbol? arg) arg
    (or (fn? arg) (set? arg)) arg
    (regex? arg) arg
    :else (rewrite arg)))

(defn- rw-core-pattern
  "Rewrite existing core pattern - recurse into args"
  [x rewrite]
  (when (core-pattern? x)
    (let [[q t & args] x]
      (if (= :match-case t)
        ;; :match-case has patterns at odd indices
        (apply list q t (map-indexed
                         (fn [i arg] (if (odd? i) (rewrite arg) arg))
                         args))
        (apply list q t (map (partial rewrite-arg rewrite) args))))))

(defn- rw-double-quoted
  "Rewrite double-quoted symbol: ''sym -> (? :val sym)"
  [x _rewrite]
  (when-let [sym (double-quoted-symbol? x)]
    (list '? :val sym)))

(defn- rw-regex
  "Rewrite regex: #\"...\" -> (? :regex #\"...\")"
  [x _rewrite]
  (when (regex? x)
    (list '? :regex x)))

(defn- rw-list-var
  "Rewrite list-var pattern: (sym pred? ...) -> core pattern"
  [x _rewrite]
  (when-let [parsed (parse-list-var x)]
    (list-var->core parsed)))

(defn- rw-matching-var
  "Rewrite quantified symbol: x*, x+, x? -> core pattern"
  [x _rewrite]
  (when-let [parsed (parse-matching-var x)]
    (matching-var->core parsed)))

(defn- rw-vector
  "Rewrite vector: [a b] -> (? :seq [...])"
  [x rewrite]
  (when (and (vector? x) (not (map-entry? x)))
    (list '? :seq (mapv #(wrap-for-seq (rewrite %)) x))))

(defn- rw-map
  "Rewrite map: {:k v} -> (? :map {:k ...})"
  [x rewrite]
  (when (map? x)
    (list '? :map (into {} (map (fn [[k v]] [k (rewrite v)])) x))))

(defn- rw-seq
  "Rewrite seq: recurse into it"
  [x rewrite]
  (when (seq? x)
    (apply list (map rewrite x))))

(defn- rw-function
  "Rewrite function: even? -> (? :pred even?)"
  [x _rewrite]
  (when (fn? x)
    (list '? :pred x)))

(defn- rw-set
  "Rewrite set: #{:a :b} -> (? :pred #{:a :b})"
  [x _rewrite]
  (when (set? x)
    (list '? :pred x)))

(defn- rw-wildcard
  "Rewrite wildcard: _ -> (? :any)"
  [x _rewrite]
  (when (= '_ x)
    '(? :any)))

(defn- rw-plain-symbol
  "Rewrite plain symbol: x -> (? :var x (? :any))"
  [x _rewrite]
  (when (plain-var-symbol? x)
    (list '? :var x '(? :any))))

(defn- rw-literal
  "Rewrite literal value: 5 -> (? :val 5)"
  [x _rewrite]
  (list '? :val x))

(def ^:private rewrite-rules
  "Ordered rewrite rules for ptn->core. First match wins."
  [rw-core-pattern
   rw-double-quoted
   rw-regex
   rw-list-var
   rw-matching-var
   rw-vector
   rw-map
   rw-seq
   rw-function
   rw-set
   rw-wildcard
   rw-plain-symbol
   rw-literal])

(defn ptn->core
  "Transform syntax sugar patterns into normalized (? :type ...) core patterns.
   This is Phase 1 of compilation - no matchers are created."
  [ptn]
  (letfn [(rewrite [x]
            (some #(% x rewrite) rewrite-rules))]
    (rewrite ptn)))

(declare core->matcher)

(defn- compile-core-arg
  "Compile an argument from a core pattern - recursively compiles nested patterns"
  [arg]
  (cond
    ;; Already a matcher - return as-is
    (matcher-type arg)
    arg
    ;; Core pattern - compile recursively
    (core-pattern? arg)
    (core->matcher arg)
    ;; Vector of children - compile each
    (vector? arg)
    (mapv compile-core-arg arg)
    ;; Map of children - compile values
    (map? arg)
    (into {} (map (fn [[k v]] [k (compile-core-arg v)])) arg)
    ;; Other values (keywords, numbers, symbols) - keep as-is
    :else
    arg))

(defn core->matcher
  "Compile a normalized core pattern to a matcher (Phase 2).
   Expects only (? :type ...) core patterns from ptn->core.
   Recursively compiles nested patterns before building the outer matcher."
  [ptn]
  (cond
    ;; Already a matcher - return as-is
    (matcher-type ptn)
    ptn

    ;; Core pattern - compile it
    (core-pattern? ptn)
    (let [[_ t & args] ptn
          ;; Recursively compile nested patterns in args first
          compiled-args (map compile-core-arg args)
          ;; Build the pattern with compiled args for core-rule processing
          compiled-ptn (apply list '? t compiled-args)]
      ;; Use core-rule to validate and build the final matcher
      (or (rule-of core-rule compiled-ptn)
          (throw (ex-info "Failed to compile core pattern" {:pattern ptn}))))

    ;; Should not happen if ptn->core ran correctly
    :else
    (throw (ex-info "Expected core pattern in Phase 2" {:pattern ptn}))))

(defn ptn->matcher
  "Compile a pattern to a matcher function.

   Compilation happens in two phases:
   1. Outer (ptn->core): Transform syntax sugar to core (? :type ...) patterns
   2. Inner (core->matcher): Compile core patterns to matchers via defmatcher specs

   Symbol handling in patterns:
   - Plain symbols (x, foo, bar) become variable bindings
   - x*, x+, x? syntax for quantified variables
   - ''sym (double-quoted) matches the literal symbol sym
   - _ is wildcard (matches anything, no binding)"
  ([ptn]
   (-> ptn ptn->core core->matcher))
  ([ptn _rules]
   ;; Custom rules not yet supported in new architecture
   (-> ptn ptn->core core->matcher)))

(defn- repeat-matcher
  "Create a repeating matcher for + and * quantifiers.
   Default is lazy (match minimum), use greedy? true to match maximum.
   Used by :matching-var defmatcher."
  [sym min-len greedy?]
  ;; Use mzrepeat with nil max-len for unbounded matching (uses actual sequence length)
  (mzrepeat wildcard min-len {:max-len nil :greedy? greedy? :sym sym}))

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; pred - predicate matching
  ;;-------------------------------------------------------------------
  ;; basic predicate
  ((rule-of core-rule (list '? :pred odd?)) (vmr 3)) ;=>> {:val 3}
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
  ;; match-case - case matching with key binding (inline pairs, no vector)
  ;;-------------------------------------------------------------------
  ;; binds matched key to symbol
  ((rule-of core-rule (list '? :match-case :even (mpred even?) :neg-one (mval -1) 'which)) (vmr 4)) ;=>>
  {:val 4 :vars '{which :even}}
  ;; falls through to second case
  ((rule-of core-rule (list '? :match-case :even (mpred even?) :neg-one (mval -1) 'which)) (vmr -1)) ;=>>
  {:val -1 :vars '{which :neg-one}}
  ;; fails when no case matches
  ((rule-of core-rule (list '? :match-case :even (mpred even?) :neg-one (mval -1) 'which)) (vmr 3)) ;=>>
  failure?
  ;; match-case with regex patterns (compiled via ptn->matcher)
  ((ptn->matcher (list '? :match-case :long-opt #"--([a-zA-Z]+)" :short-opt #"-([a-zA-Z])" 'opt-type))
   (vmr "--verbose")) ;=>>
  {:val ["--verbose" "verbose"] :vars '{opt-type :long-opt}}
  ((ptn->matcher (list '? :match-case :long-opt #"--([a-zA-Z]+)" :short-opt #"-([a-zA-Z])" 'opt-type))
   (vmr "-v")) ;=>>
  {:val ["-v" "v"] :vars '{opt-type :short-opt}}

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
  ((rule-of core-rule (list '? :sub (mpred even?) inc)) (vmr 8)) ;=>> {:val 9}
  ;; without child matcher transforms any value
  ((rule-of core-rule (list '? :sub inc)) (vmr 5)) ;=>> {:val 6}
  ;; fails when child matcher fails
  ((rule-of core-rule (list '? :sub (mpred even?) inc)) (vmr 7)) ;=>> failure?

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
  ;; fails on non-string input
  ((rule-of core-rule (list '? :regex #"\d+")) (vmr 123)) ;=>> failure?
  ;; fails when pattern doesn't match
  ((rule-of core-rule (list '? :regex #"^\d+$")) (vmr "abc")) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; regex - automatic regex pattern conversion
  ;;-------------------------------------------------------------------
  ;; regex literal automatically becomes matcher
  ((ptn->matcher #"hello") (vmr "hello")) ;=>> {:val ["hello"]}
  ;; with capture groups
  ((ptn->matcher #"(\d+)-(\d+)") (vmr "12-34")) ;=>> {:val ["12-34" "12" "34"]}
  ;; regex in map pattern (transforms value to groups)
  ((ptn->matcher {:name #"(\w+) (\w+)"}) (vmr {:name "John Doe"})) ;=>> {:val {:name ["John Doe" "John" "Doe"]}}
  ;; regex in vector pattern with variable binding
  (:vars ((ptn->matcher '[first second]) (vmr ["hello" "123"]))) ;=>> '{first "hello" second "123"}
  ;; regex validates string in vector (use vector literal, not quote)
  (:vars ((ptn->matcher ['x #"\d+"]) (vmr ["hello" "123"]))) ;=>> '{x "hello"}

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
  ;; map pattern with nested core forms
  ((ptn->matcher '{:a (? :var a (? :val 5))}) (vmr {:a 5}))) ;=>> {:val {:a 5}}


