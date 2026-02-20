(ns ^:no-doc sg.flybot.pullable.impl
  "Core pattern matching engine.

   This namespace provides the pattern matching implementation:
   - Match result types (ValMatchResult, MatchFailure)
   - Matcher primitives (mpred, mval, mmap, mseq, mor, etc.)
   - Pattern DSL and compilation (core->matcher, defmatcher)"
  (:require
   [clojure.pprint :refer [cl-format]]
   [clojure.walk :as walk]
   [clojure.zip :as zip]
   [sg.flybot.pullable.schema :as schema]
   [sg.flybot.pullable.util :refer [vars-> variable?]])
  #?(:cljs (:require-macros [sg.flybot.pullable.impl])))

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
  "Returns true if x is a MatchFailure."
  [x]
  (instance? MatchFailure x))

(extend-type MatchFailure
  IMatchResult
  (-fapply [this _f] this)
  (-bind [this _sym _v] this))

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
      (if (failure? v)
        v
        (ValMatchResult. v vars))))
  (-bind [this sym v]
    (cond
      ;; new binding
      (not (contains? vars sym))
      (ValMatchResult. val (assoc vars sym v))
      ;; unification succeeds - same value
      (= (get vars sym) v)
      this
      ;; unification fails - conflicting values
      :else
      (fail (str "binding conflict for '" sym "': had " (get vars sym) ", got " v) :bind val))))

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
  "Merge variables from `other` into `mr`, enforcing unification.
   Returns MatchFailure if same variable has conflicting values."
  [mr other]
  (if-let [vo (:vars other)]
    (reduce-kv (fn [acc sym v] (-bind acc sym v)) mr vo)
    mr))

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; vapply - applies function to val
  ;;-------------------------------------------------------------------
  ;; transforms val by applying f
  (vapply (vmr 3) inc) ;=>> {:val 4}

  ;;-------------------------------------------------------------------
  ;; -fapply - applies function to full mr
  ;;-------------------------------------------------------------------
  ;; returns new val from function result
  (-fapply (vmr 3) (fn [{:keys [val]}] (inc val))) ;=>> {:val 4}
  ;; MatchFailure implements protocol, returns self unchanged
  (-fapply (fail "test" :t 1) (fn [_] 999)) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; -bind - variable binding
  ;;-------------------------------------------------------------------
  ;; adds new binding to vars
  (-bind (vmr 3) 'a 4) ;=>> {:vars {'a 4}}
  ;; unification with same value succeeds
  (-bind (vmr 3 {'a 4}) 'a 4) ;=>> {:vars {'a 4}}
  ;; unification conflict returns failure
  (-bind (vmr 3 {'a 4}) 'a 5) ;=>> failure?
  ;; MatchFailure implements protocol, returns self unchanged
  (-bind (fail "test" :t 1) 'x 2) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; merge-vars - merge variable bindings with unification
  ;;-------------------------------------------------------------------
  ;; merges vars from other into mr
  (merge-vars (vmr 1 {'a 1}) (vmr 2 {'b 2})) ;=>> {:val 1 :vars {'a 1 'b 2}}
  ;; handles nil vars gracefully
  (merge-vars (vmr 1 {'a 1}) (vmr 2)) ;=>> {:val 1 :vars {'a 1}}
  ;; unification succeeds when same var has same value
  (merge-vars (vmr 1 {'x 5}) (vmr 2 {'x 5})) ;=>> {:val 1 :vars {'x 5}}
  ;; unification fails when same var has different values
  (merge-vars (vmr 1 {'x 5}) (vmr 2 {'x 6})) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; deeper-failure - select best failure
  ;;-------------------------------------------------------------------
  ;; returns failure with greater depth
  (deeper-failure (fail "a" :t 1 [] 1) (fail "b" :t 2 [] 2)) ;=>> {:depth 2}
  ;; handles nil arguments
  (deeper-failure nil (fail "b" :t 2)) ;=>> failure?
  (deeper-failure (fail "a" :t 1) nil) ;=>> failure?

  ;;-------------------------------------------------------------------
  ;; nest-failure - add path segment
  ;;-------------------------------------------------------------------
  ;; adds segment to path and increments depth
  (nest-failure (fail "reason" :t "val") :key) ;=>> {:path [:key] :depth 1}
  ;; prepends to existing path
  (nest-failure (fail "reason" :t "val" [:inner] 1) :outer)) ;=>> {:path [:outer :inner] :depth 2})

;;=============================================================================
;; SECTION 2: Matcher Primitives
;;=============================================================================

;;## Implementation of matchers
;;
;; A matcher is a function that takes a mr and returns a mr,
;; or a MatchFailure if not matching.

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
  (matcher :var
           (fn [mr]
             (let [mr' (child mr)]
               (if (failure? mr')
                 mr'
                 (-bind mr' sym (:val mr')))))
           ::child-type (matcher-type child)))

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
  "a matcher tests `matchers` and returns the first success, or the best (deepest) failure.
   Returns identity matcher if `matchers` is empty or nil."
  [matchers]
  (if (seq matchers)
    (matcher :or
             (fn [mr]
               (loop [[m & rest] matchers
                      best-fail nil]
                 (if m
                   (let [result (m mr)]
                     (if (failure? result)
                       (recur rest (deeper-failure best-fail result))
                       result))
                   (or best-fail (fail "no matchers to try" :or (:val mr)))))))
    identity))

(defn mcase
  "a matcher tests sequentially `kv-matchers`, which is a key to a matcher,
  bind the successful key to `sym`"
  [kv-matchers sym]
  (matcher :case
           (fn [mr]
             (loop [kvs (partition 2 kv-matchers)
                    best-fail nil]
               (if-let [[[k mch] & rest] (seq kvs)]
                 (let [result (mch mr)]
                   (if (failure? result)
                     (recur rest (deeper-failure best-fail result))
                     (cond-> result sym (-bind sym k))))
                 (or best-fail (fail "no matchers matched" :case (:val mr))))))))

(defn mnot
  "a matcher that succeeds when `child` fails, and fails when `child` succeeds"
  [child]
  (matcher :not
           (fn [mr]
             (if (failure? (child mr))
               mr
               (fail "negation failed: child matcher succeeded" :not (:val mr))))))

^:rct/test
(comment
  ;; mor returns first successful match (short-circuits)
  (map (mor [(mpred neg?) (mpred even?)]) [(vmr -1) (vmr 2) (vmr 3)]) ;=>>
  [{} {} failure?]
  ;; mor with empty vector returns mr unmodified
  ((mor []) (vmr 42)) ;=>>
  {:val 42 :vars {}}
  ;; mor with nil returns mr unmodified
  ((mor nil) (vmr 42)) ;=>>
  {:val 42 :vars {}}
  ;; mcase tries matchers in order, binds key of first success to symbol
  ((mcase [0 (mpred even?) 1 (mpred odd?)] 'b) (vmr 3)) ;=>> 
  {:vars {'b 1}})

;;## Map matcher

(defn mmap
  "A matcher that matches on `matcher-map`, a map of keys to matchers.
   Supports maps and ILookup implementations (for lazy data sources).
   For maps: preserves unmatched keys in :val (passthrough semantics).
   For ILookup: returns only matched keys (can't enumerate all keys)."
  [matcher-map]
  (matcher :map
           (fn [mr]
             (let [m (:val mr)
                   is-map? (map? m)]
               (if-not #?(:clj  (instance? clojure.lang.ILookup m)
                          :cljs (satisfies? ILookup m))
                 (fail (str "expected map, got " (type m)) :map m)
                 (reduce
                  (fn [mr' [k mch]]
                    (let [result (mch (vmr (get m k)))]
                      (if (failure? result)
                        (reduced (nest-failure result k))
                        (-> mr'
                            (update :val assoc k (:val result))
                            (merge-vars result)))))
                  ;; For maps: preserve unmatched keys (passthrough)
                  ;; For ILookup: start with empty map (can't enumerate)
                  (if is-map? mr (assoc mr :val {}))
                  matcher-map))))))

^:rct/test
(comment
  ;; mmap succeeds when all key matchers succeed, collects vars
  (def mm (mmap {:a (mvar 'a (mval 4)) :b (mpred even?)}))
  (mm (vmr {:a 4 :b 0})) ;=>> {:val {:a 4 :b 0} :vars {'a 4}}
  ;; mmap fails when any key matcher fails
  (mm (vmr {:a 4 :b 3})) ;=>>
  failure?

  ;; mmap works with ILookup implementations (not just maps)
  ;; This is essential for lazy data sources
  (def test-ilookup
    (reify clojure.lang.ILookup
      (valAt [_ k] (get {:a 1 :b 2} k))
      (valAt [_ k nf] (get {:a 1 :b 2} k nf))))
  ((mmap {:a (mvar 'x wildcard)}) (vmr test-ilookup)) ;=>>
  {:val {:a 1} :vars {'x 1}}

  ;; mmap with non-keyword keys (indexed lookup)
  (def idx-lookup
    (reify clojure.lang.ILookup
      (valAt [_ k] (get {{:id 1} {:name "Alice"}} k))
      (valAt [_ k nf] (get {{:id 1} {:name "Alice"}} k nf))))
  ((mmap {{:id 1} (mvar 'result wildcard)}) (vmr idx-lookup)) ;=>>
  {:val {{:id 1} {:name "Alice"}} :vars {'result {:name "Alice"}}})

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
                     (let [nv (:val result)
                           z' (if (= node nv) zip (zip/replace zip nv))]
                       (-> zmr
                           (assoc :val (zip-advance z'))
                           (merge-vars result))))))))))

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
  (matcher :subseq
           (fn [zmr]
             (let [zip (:val zmr)]
               (if (zip/end? zip)
                 zmr
                 (fail (str "expected end of sequence, but found: " (pr-str (zip/node zip))) :subseq (zip/node zip)))))))

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
        (fail "no length worked" :subseq items)
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
                                 (matcher :subseq
                                          (fn [zmr]
                                            ;; Collect all matching elements up to max-len (nil = unbounded)
                                            (let [[items _final-z] (try-match-elements child zmr max-len)
                                                  item-count (count items)]
                                              (if (< item-count min-len)
                                                (fail (str "need at least " min-len " elements, got " item-count) :subseq items)
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
  ((mseq [(mzone (mvar 'first wildcard)) (mzfilter even? {:sym 'rest-evens})]) (vmr [1 2 3 4 5 6])) ;=>>
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
  ((mseq [(mzone (mvar 'head wildcard)) (mzfirst even? {:sym 'first-even})]) (vmr [1 3 4 5 6])) ;=>>
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
;; SECTION 3: Pattern DSL and Compilation
;;=============================================================================

;;-----------------------------------------------------------------------------
;; Customizable Symbol/Form Resolution
;;-----------------------------------------------------------------------------
;; Supports automatic SCI detection for sandboxed evaluation.
;; When SCI (org.babashka/sci) is on the classpath, it's used by default.
;; Otherwise falls back to clojure.core/resolve and eval (CLJ only).
;;
;; Users can override via :resolve and :eval-fn options in compile-pattern,
;; or by binding *resolve-sym* and *eval-form* directly.

;; Check at load time if SCI is available (no runtime penalty)
;; CLJ: try to require sci.core
;; CLJS: SCI must be configured explicitly via :resolve/:eval-fn options
#?(:clj
   (def ^:private sci-available?
     (try
       (require 'sci.core)
       true
       (catch Exception _ false)))
   :cljs
   (def ^:private sci-available? false))

;; Cached SCI context (created once at load time if SCI available)
#?(:clj
   (def ^:private sci-ctx
     (when sci-available?
       ((requiring-resolve 'sci.core/init) {})))
   :cljs
   (def ^:private sci-ctx nil))

#?(:clj
   (defn- sci-eval
     "Evaluate form using SCI. Only called when sci-available? is true."
     [form]
     ((requiring-resolve 'sci.core/eval-form) sci-ctx form)))

(def ^:dynamic *resolve-sym*
  "Custom symbol resolver: (fn [sym] -> var-or-value).
   When nil, uses SCI if available, else clojure.core/resolve (CLJ only).
   Bind via :resolve option in compile-pattern."
  nil)

(def ^:dynamic *eval-form*
  "Custom form evaluator: (fn [form] -> value).
   When nil, uses SCI if available, else clojure.core/eval (CLJ only).
   Bind via :eval-fn option in compile-pattern."
  nil)

(defn- fn-form?
  "Check if x is a (fn ...) or (fn* ...) form."
  [x]
  (and (seq? x) (contains? #{'fn 'fn*} (first x))))

(defn- default-resolve
  "Default symbol resolver. Uses SCI if available, else clojure.core/resolve."
  [sym]
  #?(:clj (if sci-available?
            (sci-eval sym)
            (resolve sym))
     :cljs (throw (ex-info "No symbol resolver. Add SCI to dependencies or provide :resolve option."
                           {:symbol sym}))))

(defn- default-eval
  "Default form evaluator. Uses SCI if available, else clojure.core/eval."
  [form]
  #?(:clj (if sci-available?
            (sci-eval form)
            (eval form))
     :cljs (throw (ex-info "No form evaluator. Add SCI to dependencies or provide :eval-fn option."
                           {:form form}))))

(defn resolve-fn
  "Resolve a value to a function. Handles:
   - Functions: returned as-is
   - Symbols: resolved via *resolve-sym*, SCI (if available), or clojure.core/resolve
   - (fn ...) or (fn* ...) forms: evaluated via *eval-form*, SCI, or clojure.core/eval

   SCI is auto-detected: add org.babashka/sci to your deps for sandboxed eval.
   Override via :resolve and :eval-fn options in compile-pattern."
  [x]
  (cond
    (fn? x) x

    (symbol? x)
    (let [resolve-f (or *resolve-sym* default-resolve)
          v (resolve-f x)]
      (if v
        (let [f (if (var? v) @v v)]
          (if (fn? f)
            f
            (throw (ex-info "Symbol does not resolve to a function"
                            {:symbol x :resolved f}))))
        (throw (ex-info "Cannot resolve symbol" {:symbol x}))))

    (fn-form? x)
    (let [eval-f (or *eval-form* default-eval)
          f (eval-f x)]
      (if (fn? f)
        f
        (throw (ex-info "Form did not evaluate to a function"
                        {:form x :result f}))))

    :else (throw (ex-info "Expected function, symbol, or fn form" {:value x :type (type x)}))))

^:rct/test
(comment
  ;; resolve-fn: resolves symbols and fn forms to functions at compile time

  ;; function passes through unchanged
  (fn? (resolve-fn odd?)) ;=> true
  ;; symbol resolves to function
  (fn? (resolve-fn 'odd?)) ;=> true
  ;; resolved function works correctly
  ((resolve-fn 'odd?) 3) ;=> true
  ((resolve-fn 'odd?) 4) ;=> false
  ;; non-existent symbol throws
  (try (resolve-fn 'nonexistent-fn-12345) (catch Exception e :threw)) ;=> :threw

  ;; fn forms are evaluated to create functions
  (fn? (resolve-fn '(fn [x] (odd? x)))) ;=> true
  ((resolve-fn '(fn [x] (odd? x))) 3) ;=> true
  ;; fn* forms (from #(...) reader macro) also work
  (fn? (resolve-fn '(fn* [p1__1#] (odd? p1__1#)))) ;=> true

  ;; Symbol resolution in patterns:
  ;; '(? :pred odd?) now works - symbol is resolved to function
  (let [m (compile-pattern '(? :pred odd?))]
    (failure? (m (vmr 3)))) ;=> false
  (let [m (compile-pattern '(? :pred odd?))]
    (failure? (m (vmr 4)))) ;=> true

  ;; Anonymous functions in patterns:
  ;; '(? :pred #(zero? (mod % 3))) works - fn form is evaluated
  (let [m (compile-pattern '(? :pred #(zero? (mod % 3))))]
    (failure? (m (vmr 3)))) ;=> false
  (let [m (compile-pattern '(? :pred #(zero? (mod % 3))))]
    (failure? (m (vmr 4)))) ;=> true
  (let [m (compile-pattern '(? :pred #(zero? (mod % 3))))]
    (failure? (m (vmr 6)))) ;=> false

  ;; :when option also works with symbols and fn forms
  (let [m (compile-pattern (list '?x :when 'odd?))]
    (:vars (m (vmr 3)))) ;=>> {'x 3}
  (let [m (compile-pattern (list '?x :when 'odd?))]
    (failure? (m (vmr 4)))) ;=> true

  ;; Custom resolver/evaluator via dynamic vars
  ;; Useful for sandboxing (SCI) or CLJS compatibility
  (binding [*resolve-sym* (fn [sym] (when (= sym 'my-pred) #(> % 10)))
            *eval-form* (fn [form] (clojure.core/eval form))]
    (fn? (resolve-fn 'my-pred))) ;=> true
  (binding [*resolve-sym* (fn [sym] (when (= sym 'my-pred) #(> % 10)))]
    ((resolve-fn 'my-pred) 15)) ;=> true

  ;; Custom resolver/evaluator via compile-pattern options
  (let [my-resolve (fn [sym] (case sym pos? pos? neg? neg? nil))
        m (compile-pattern '(? :pred pos?) {:resolve my-resolve})]
    (failure? (m (vmr 5)))) ;=> false
  (let [my-resolve (fn [sym] (case sym pos? pos? neg? neg? nil))
        m (compile-pattern '(? :pred neg?) {:resolve my-resolve})]
    (failure? (m (vmr 5))))) ;=> true)

(def ^:private matcher-specs* (atom {}))

(defn defmatcher
  "Define a matcher type specification.
   type - keyword like :pred, :val, etc.
   args - human-readable argument format string
   parse - matcher that validates and extracts arguments
   make - a (fn [vars] ...) that creates the matcher from parsed vars
   opts - optional map with :subseq? true for subsequence matchers"
  ([type args parse make]
   (swap! matcher-specs* assoc type
          {:args args
           :parse parse
           :make make}))
  ([type args parse make opts]
   (swap! matcher-specs* assoc type
          (merge {:args args
                  :parse parse
                  :make make}
                 opts))))

(defmatcher :pred "(? :pred <fn>)"
  (mzone (mvar 'f wildcard))
  (vars-> [f] (mpred (resolve-fn f))))

(defmatcher :val "(? :val <value>)"
  (mzone (mvar 'val wildcard))
  (vars-> [val] (mval val)))

(defmatcher :any "(? :any)"
  mterm  ;; no args expected
  (vars-> [] wildcard))

(defmatcher :map "(? :map <key> <pattern>...)"
  ;; Collect key-pattern pairs as flat variadic args
  ;; Keys can be keywords (record access) or any value (indexed lookup)
  (mzcollect (constantly true) 'pairs 0)
  (vars-> [pairs]
          (when-not (and (even? (count pairs))
                         (every? matcher-type (take-nth 2 (rest pairs))))
            (throw (ex-info "Invalid :map args: expected alternating keys and matchers"
                            {:pairs pairs})))
          (mmap (apply hash-map pairs))))

(defmatcher :one "(? :one <matcher>)"
  (mzone (mvar 'child (mpred matcher-type)))
  (vars-> [child] (mzone child))
  {:subseq? true})

(defmatcher :optional "(? :optional <matcher>)"
  (mzone (mvar 'child (mpred matcher-type)))
  (vars-> [child] (mzoption child))
  {:subseq? true})

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

(defmatcher :repeat "(? :repeat <matcher> :min <n> [:max <n>] [:as <sym>] [:greedy])"
  ;; Collect matcher and all remaining args
  (mzsubseq [(mzone (mvar 'child (mpred matcher-type)))
             (mzcollect (constantly true) 'rest 0)]
            nil)
  (vars-> [child rest]
          (let [kw-args (parse-keyword-args rest)
                min-len (or (:min kw-args) 0)
                max-len (:max kw-args)
                sym (:as kw-args)
                greedy? (contains? kw-args :greedy)]
            (mzrepeat child min-len {:max-len max-len :greedy? greedy? :sym sym})))
  {:subseq? true})

(defmatcher :seq "(? :seq <matcher>...)"
  ;; Collect matchers as variadic args
  (mzcollect matcher-type 'children 0)
  (vars-> [children] (mseq children)))

(defmatcher :term "(? :term)"
  mterm
  (vars-> [] mterm)
  {:subseq? true})

(defmatcher :var "(? :var <sym> <matcher>)"
  (mzsubseq [(mzone (mvar 'sym (mpred symbol?)))
             (mzone (mvar 'child (mpred matcher-type)))]
            nil)
  (vars-> [sym child] (mvar sym child)))

(defmatcher :or "(? :or <matcher>...)"
  (mzcollect matcher-type 'alternatives)
  (vars-> [alternatives] (mor alternatives)))

(defmatcher :not "(? :not <matcher>)"
  (mzone (mvar 'child (mpred matcher-type)))
  (vars-> [child] (mnot child)))

(defmatcher :-> "(? :-> <matcher>...)"
  (mzcollect matcher-type 'steps)
  (vars-> [steps] (mchain steps)))

(defmatcher :case "(? :case [<sym>] <key> <matcher>...)"
  ;; First optional sym, then collect key-matcher pairs
  (mzsubseq [(mzoption (mvar 'sym (mpred symbol?)))
             (mzcollect (constantly true) 'items 2)]
            nil)
  (vars-> [sym items]
    ;; Validate structure: even count, matchers at odd indices
          (when-not (and (even? (count items))
                         (every? matcher-type (take-nth 2 (rest items))))
            (throw (ex-info (str "Invalid :case arguments. Expected: "
                                 "(? :case [<sym>] <key> <matcher>...)")
                            {:items items})))
          (mcase (vec items) sym)))

(defmatcher :filter "(? :filter <pred> [<sym>])"
  (mzsubseq [(mzone (mvar 'pred (mpred ifn?)))
             (mzoption (mvar 'sym (mpred symbol?)))]
            nil)
  (vars-> [pred sym] (mzfilter pred {:sym sym}))
  {:subseq? true})

(defmatcher :first "(? :first <pred> [<sym>])"
  (mzsubseq [(mzone (mvar 'pred (mpred ifn?)))
             (mzoption (mvar 'sym (mpred symbol?)))]
            nil)
  (vars-> [pred sym] (mzfirst pred {:sym sym}))
  {:subseq? true})

(defmatcher :sub "(? :sub [<matcher>] <fn>)"
  (mzsubseq [(mzoption (mvar 'child (mpred matcher-type)))
             (mzone (mvar 'f (mpred ifn?)))]
            nil)
  (vars-> [child f]
          (mchain [(or child wildcard) (msub f)])))

(defmatcher :update "(? :update <fn>)"
  (mzone (mvar 'f (mpred ifn?)))
  (vars-> [f]
          (matcher :update
                   (fn [mr]
                     (assoc mr :val (f (:val mr)))))))

(defmatcher :regex "(? :regex <pattern>)"
  (mzone (mvar 'pattern (mpred regex?)))
  (vars-> [pattern]
          (matcher :regex
                   (fn [mr]
                     (let [v (:val mr)]
                       (if-not (string? v)
                         (fail (str "expected string for regex match, got " (type v)) :regex v)
                         (if-let [result (re-matches pattern v)]
                     ;; re-matches returns: string (no groups), or vector [full-match & groups]
                           (assoc mr :val (if (string? result) [result] result))
                           (fail (str "regex " pattern " did not match: " (pr-str v)) :regex v))))))))

(defn- get-spec
  "Get spec for matcher type, throws if unknown"
  [t]
  (or (get @matcher-specs* t)
      (let [valid-types (keys @matcher-specs*)]
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

(defn- core-pattern?
  "Check if x is a core (? :type ...) pattern"
  [x]
  (and (seq? x) (= '? (first x)) (keyword? (second x))))

;;-----------------------------------------------------------------------------
;; Pattern Rewriting (Phase 1)
;;
;; Rewrite rules transform syntax sugar into core (? :type ...) patterns.
;; A rewrite rule is a function: pattern -> core-pattern | nil
;;-----------------------------------------------------------------------------

(defn rewrite-pattern
  "Rewrite syntax sugar to core patterns.
   Uses postwalk to apply rules bottom-up (inner forms first).
   Skips descending into fn forms (they're kept as-is for resolve-fn).
   For maps, only walks values (keys are literal lookup keys).
   Each rule is a fn: pattern -> core-pattern or nil"
  [ptn rules]
  (let [apply-rules (fn [x] (or (some #(% x) rules) x))]
    ((fn walk [x]
       (cond
         (fn-form? x) x
         (and (map? x) (not (record? x))) (apply-rules (update-vals x walk))
         :else (apply-rules (walk/walk walk identity x))))
     ptn)))

(defn- wrap-map-value
  "Wrap a pattern value for use in (? :map ...).
   - Core patterns pass through
   - Other values get wrapped in (? :val ...)"
  [v]
  (if (core-pattern? v)
    v
    (list '? :val v)))

(defn map-rewrite
  "Rewrite map literal {} to (? :map ...) core pattern.
   Wraps non-pattern values in (? :val ...).
   Returns nil if not applicable."
  [x]
  (when (and (map? x) (not (record? x)))
    (apply list '? :map (mapcat (fn [[k v]] [k (wrap-map-value v)]) x))))

(defn- subseq-type?
  "Check if a matcher type keyword has :subseq? true in its spec"
  [t]
  (some-> @matcher-specs* (get t) :subseq?))

(defn- subseq-pattern?
  "Check if x is a subseq core pattern like (? :one ...), (? :repeat ...), etc.
   Also recognizes chains (? :-> ...) where the first element is a subseq pattern."
  [x]
  (and (seq? x)
       (= '? (first x))
       (let [t (second x)]
         (or (subseq-type? t)
             ;; Chain starting with a subseq pattern is also a subseq pattern
             (and (= :-> t)
                  (subseq-pattern? (nth x 2 nil)))))))

(defn- wrap-seq-element
  "Wrap a pattern element for use in (? :seq ...).
   - Subseq patterns (? :one ...), (? :repeat ...) etc. pass through
   - Other patterns get wrapped in (? :one ...)
   - Literal values get wrapped in (? :one (? :val ...))"
  [elem]
  (cond
    ;; Already a subseq pattern - pass through
    (subseq-pattern? elem)
    elem
    ;; Core pattern but not subseq - wrap in :one
    (core-pattern? elem)
    (list '? :one elem)
    ;; Literal value - wrap in :one :val
    :else
    (list '? :one (list '? :val elem))))

(defn vector-rewrite
  "Rewrite vector [] to (? :seq ...) core pattern.
   Automatically wraps non-subseq elements in (? :one ...).
   Appends (? :term) to ensure entire sequence is consumed.
   Returns nil if not applicable."
  [x]
  (when (and (vector? x) (not (map-entry? x)))
    (apply list '? :seq (concat (map wrap-seq-element x) ['(? :term)]))))

(defn- extended-var-form?
  "Check if x looks like an extended var form (?x :opt val ...).
   The first element may already be rewritten by matching-var-rewrite."
  [x]
  (and (seq? x)
       (let [fst (first x)]
         (or ;; Original form: (?x :opt ...)
          (variable? fst)
             ;; After matching-var-rewrite: ((? :var ...) :opt ...)
          (and (seq? fst) (= '? (first fst)) (= :var (second fst)))
             ;; Rewritten wildcard: ((? :any) :skip ...)
          (= fst '(? :any))
             ;; Rewritten quantified var: ((? :repeat ...) :take ...)
          (and (seq? fst) (= '? (first fst)) (= :repeat (second fst)))))))

(defn list-rewrite
  "Rewrite list () to (? :seq ...) core pattern.
   Same as vector-rewrite but for lists.
   Returns nil if not applicable, if it's a core pattern, or extended var form.
   Note: fn forms are already skipped by rewrite-pattern, so no check needed here."
  [x]
  (when (and (seq? x)
             (not (core-pattern? x))
             (not (extended-var-form? x)))
    (apply list '? :seq (concat (map wrap-seq-element x) ['(? :term)]))))

(defn set-rewrite
  "Throws an error for set patterns - sets are not supported as patterns.
   Sets in schemas represent enums, but pattern syntax doesn't support them."
  [x]
  (when (set? x)
    (throw (ex-info "Sets are not supported as patterns. Use (? :or ...) for alternatives or (? :pred #(contains? #{...} %)) for set membership."
                    {:set x}))))

(def ^:private forbidden-var-chars
  "Characters forbidden in matching variable names"
  #"[\?\+\*\!]")

(defn parse-matching-var
  "Parse a quantified variable symbol like ?x?, ?x+, ?x*, ?x+!, ?x*!.
   Variables must start with ? prefix. Var names cannot contain ?, +, *, !.
   Returns map with :sym, :quantifier, :greedy? or nil if not a matching-var."
  [v]
  (when (symbol? v)
    ;; Regex: ? followed by var-name (no forbidden chars), quantifier, optional greedy
    (when-let [[_ base suffix greedy] (re-matches #"\?([^\s\?\+\*\!]+)([\?\+\*])(\!?)" (name v))]
      {:sym (when (not= "_" base) (symbol (str "?" base)))
       :quantifier (case suffix "?" :optional "+" :one-or-more "*" :zero-or-more)
       :greedy? (= "!" greedy)})))

(defn matching-var-rewrite
  "Rewrite matching variables:
   - ?x -> (? :var x (? :any))
   - ?_ -> (? :any)
   - ?x? -> (? :optional (? :var x (? :any)))
   - ?x+ -> (? :repeat (? :any) :min 1 :max nil :as x)
   - ?x* -> (? :repeat (? :any) :min 0 :max nil :as x)
   - ?x+!, ?x*! -> same with :greedy
   Bare ? is not a matching var - it's the core pattern marker.
   Returns nil if not applicable."
  [x]
  (when (symbol? x)
    (let [nm (name x)]
      (when (and (= \? (first nm))
                 (> (count nm) 1))  ;; bare ? is not a matching var
        (if-let [{:keys [sym quantifier greedy?]} (parse-matching-var x)]
          ;; Quantified var: ?x?, ?x+, ?x*, etc.
          (let [var-sym (when sym (symbol (subs (name sym) 1)))
                inner '(? :any)]
            (case quantifier
              :optional
              (if var-sym
                (list '? :optional (list '? :var var-sym inner))
                (list '? :optional inner))

              (:one-or-more :zero-or-more)
              (let [min-len (if (= quantifier :one-or-more) 1 0)
                    base (list '? :repeat inner :min min-len :max nil)]
                (cond-> base
                  var-sym (concat [:as var-sym])
                  greedy? (concat [:greedy])
                  true seq))))

          ;; Simple var: ?x or ?_ (must not contain forbidden chars)
          (let [var-name (subs nm 1)]
            (when-not (re-find forbidden-var-chars var-name)
              (if (= "_" var-name)
                '(? :any)
                (list '? :var (symbol var-name) '(? :any))))))))))

;;-----------------------------------------------------------------------------
;; Extended Matching Variables: (?var :opt1 val1 :opt2 val2 ...)
;;-----------------------------------------------------------------------------

(def var-option-handlers
  "Registry of option handlers for extended matching vars.
   Each handler: (fn [option-value] -> pattern-step-to-chain)
   Users can extend by adding to this atom."
  (atom {}))

(defn register-var-option!
  "Register an option handler for extended matching vars.
   The handler receives the option value and returns a pattern to chain."
  [key handler]
  (swap! var-option-handlers assoc key handler))

;; NOTE: Quoted patterns like '(?x :when odd?) contain symbols, not functions.
;; In CLJ, symbols and fn forms are resolved automatically via resolve-fn.
;; For CLJS or sandboxed evaluation, provide :resolve and :eval-fn options to
;; compile-pattern (e.g., using SCI). See *resolve-sym* and *eval-form* docs.

;; Built-in chain option: :when - adds a predicate check
(register-var-option! :when
                      (fn [pred]
                        (list '? :pred pred)))

;; Built-in chain option: :default - provides default for nil values
(register-var-option! :default
                      (fn [default-val]
                        (list '? :sub (fn [x] (if (nil? x) default-val x)))))

;;-----------------------------------------------------------------------------
;; Structural Option Registry
;;-----------------------------------------------------------------------------
;; Structural options modify the base pattern itself (vs chain options which
;; post-process the result). Handler signature:
;;   (fn [value context] -> pattern)
;; Context map: {:inner pattern, :var-sym symbol, :min-len int, :greedy? bool}

(defonce structural-option-handlers* (atom {}))

(defn register-structural-option!
  "Register a structural option that modifies the base pattern.
   Handler receives [value context] and returns the new pattern."
  [k handler]
  (swap! structural-option-handlers* assoc k handler))

;; :skip - consume exactly N elements (for wildcards)
(register-structural-option! :skip
                             (fn [n {:keys [inner]}]
                               (list '? :repeat inner :min n :max n)))

;; :take - limit collection to N elements, implies greedy
;; Returns pattern with :wrap-var? false since :as handles binding
(register-structural-option! :take
                             (fn [n {:keys [inner var-sym min-len]}]
                               {:inner (cond-> (list '? :repeat inner :min (or min-len 0) :max n)
                                         var-sym (concat [:as var-sym])
                                         true (concat [:greedy])
                                         true seq)
                                :wrap-var? false}))

(defn- apply-structural-options
  "Apply structural options that modify the base pattern.
   Handler can return a pattern or a map with :inner and optional :wrap-var?"
  [ctx opt-pairs]
  (reduce (fn [ctx [k v]]
            (if-let [handler (get @structural-option-handlers* k)]
              (let [result (handler v ctx)]
                (if (map? result)
                  (merge ctx result)
                  (assoc ctx :inner result)))
              ctx))
          ctx
          opt-pairs))

(defn- apply-chain-options
  "Apply chain options (like :when) that post-process the result."
  [base opt-pairs]
  (let [opt-patterns (for [[k v] opt-pairs
                           :let [handler (get @var-option-handlers k)]
                           :when handler]
                       (handler v))]
    (if (seq opt-patterns)
      (apply list '? :-> base opt-patterns)
      base)))

(defn- structural-option? [k]
  (contains? @structural-option-handlers* k))

(defn- chain-option? [k]
  (contains? @var-option-handlers k))

(defn- parse-var-context
  "Parse a var symbol or already-rewritten pattern into context map.
   Returns {:var-sym symbol, :inner pattern, :min-len int, :greedy? bool} or nil."
  [fst]
  (cond
    ;; Quantified var: ?x*, ?x+, ?x?, ?x*!, ?x+!
    (and (variable? fst) (parse-matching-var fst))
    (let [{:keys [sym quantifier greedy?]} (parse-matching-var fst)
          var-sym (when sym (symbol (subs (name sym) 1)))]
      (case quantifier
        :zero-or-more {:var-sym var-sym :inner '(? :any) :min-len 0 :greedy? greedy?}
        :one-or-more  {:var-sym var-sym :inner '(? :any) :min-len 1 :greedy? greedy?}
        :optional     {:var-sym var-sym :inner '(? :any) :min-len 0 :max-len 1}
        nil))

    ;; Simple var: ?x, ?_ (bare ? is not a var - it's the core pattern marker)
    (and (variable? fst)
         (> (count (name fst)) 1)
         (not (re-find forbidden-var-chars (subs (name fst) 1))))
    (let [var-name (subs (name fst) 1)]
      {:var-sym (when (not= "_" var-name) (symbol var-name))
       :inner '(? :any)})

    ;; Already-rewritten: (? :any), (? :var ...), (? :repeat ...)
    (and (seq? fst) (= '? (first fst)))
    (case (second fst)
      :any    {:inner '(? :any)}
      :var    (let [args (drop 2 fst)]
                {:var-sym (first args) :inner (second args)})
      :repeat (let [args (drop 2 fst)
                    kw-args (apply hash-map (rest args))]
                {:var-sym (get kw-args :as)
                 :inner (first args)
                 :min-len (get kw-args :min 0)
                 :max-len (get kw-args :max)
                 :greedy? (contains? kw-args :greedy)})
      nil)

    :else nil))

(defn extended-var-rewrite
  "Rewrite extended var forms (?var :opt val ...) to patterns.
   Options are processed in two phases:
   1. Structural options (:take, :skip) - modify the base pattern
   2. Chain options (:when, :default) - post-process the result
   Returns nil if not applicable."
  [form]
  (when (seq? form)
    (when-let [ctx (parse-var-context (first form))]
      (let [opt-pairs (partition 2 (rest form))
            struct-opts (filter (fn [[k _]] (structural-option? k)) opt-pairs)
            chain-opts (filter (fn [[k _]] (chain-option? k)) opt-pairs)
            ;; Apply structural options to build the inner pattern
            {:keys [var-sym inner wrap-var?] :or {wrap-var? true}}
            (apply-structural-options ctx struct-opts)
            ;; Apply chain options
            inner (apply-chain-options inner chain-opts)]
        ;; Only wrap with (? :var ...) for simple vars, not repeat patterns
        (if (and var-sym wrap-var?)
          (list '? :var var-sym inner)
          inner)))))

;;-----------------------------------------------------------------------------
;; Schema Support
;;-----------------------------------------------------------------------------
;;
;; Schemas validate pattern structure at compile time. Schema vocabulary:
;;
;; Type keywords (structural):
;;   :map :seq :string :number :keyword :symbol :boolean :any
;;
;; Literal/Enum/Union/Optional:
;;   [:= value]     - literal, type inferred from value
;;   #{:a :b :c}    - enum (set), type inferred from elements
;;   [:or s1 s2]    - union, common type or :any if mixed
;;   [:optional s]  - nullable, type inferred from inner schema
;;
;; Map schemas:
;;   {:key schema}              - record (specific fields)
;;   [:map-of key-type val-type] - dictionary (homogeneous key/value types)
;;
;; Seq schemas:
;;   [:element-type]         - homogeneous seq
;;   [:tuple type1 type2]    - positional tuple
;;
;; Extensible via register-schema-rule! using match-fn patterns.

;;-----------------------------------------------------------------------------
;; Schema Support (delegated to schema namespace)
;;-----------------------------------------------------------------------------

(def register-schema-rule!
  "Register a schema rule. Rule: (schema) -> {:type :child-schema :valid-keys} | nil"
  schema/register-schema-rule!)

(def get-schema-info
  "Get schema info: {:type :child-schema :valid-keys}"
  schema/get-schema-info)

(def infer-schema-type
  "Infer type keyword from schema."
  schema/infer-schema-type)

(defn- validate-pattern-schema!
  "Validate pattern against schema. Throws on violation."
  [ptn s]
  (schema/validate-pattern-schema! ptn s core-pattern?))

;;-----------------------------------------------------------------------------
;; Pattern Compilation (Phase 2)
;;-----------------------------------------------------------------------------

(defn core->matcher
  "Compile a core pattern to a matcher.
   Expects only (? :type ...) core patterns (use rewrite-pattern first for syntax sugar).
   Uses postwalk to compile bottom-up, avoiding stack overflow on deep nesting."
  [ptn]
  (walk/postwalk (fn [x]
                   (cond
                     ;; Already a matcher - return as-is
                     (matcher-type x) x
                     ;; Core pattern with compiled children - build matcher
                     (core-pattern? x)
                     (or (rule-of core-rule x)
                         (throw (ex-info "Failed to compile core pattern" {:pattern x})))
                     ;; Other values - keep as-is (literals, etc.)
                     :else x))
                 ptn))

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; map-rewrite - transforms {} to (? :map ...)
  ;;-------------------------------------------------------------------
  ;; basic map rewrite - core patterns pass through
  (map-rewrite {:a '(? :any)}) ;=>>
  '(? :map :a (? :any))
  ;; literal values get wrapped in (? :val ...)
  (map-rewrite {:a 5}) ;=>>
  '(? :map :a (? :val 5))
  ;; nested maps
  (rewrite-pattern {:a {:b '(? :any)}} [map-rewrite]) ;=>>
  '(? :map :a (? :map :b (? :any)))
  ;; returns nil for non-maps
  (map-rewrite [1 2 3]) ;=> nil
  ;; returns nil for records
  (map-rewrite (vmr 1)) ;=> nil

  ;;-------------------------------------------------------------------
  ;; vector-rewrite - transforms [] to (? :seq ... (? :term))
  ;;-------------------------------------------------------------------
  ;; wraps literal values in (? :one (? :val ...)), appends (? :term)
  (vector-rewrite [3]) ;=>>
  '(? :seq (? :one (? :val 3)) (? :term))
  ;; wraps non-subseq patterns in (? :one ...)
  (vector-rewrite ['(? :any)]) ;=>>
  '(? :seq (? :one (? :any)) (? :term))
  ;; subseq patterns pass through unwrapped
  (vector-rewrite ['(? :one (? :any))]) ;=>>
  '(? :seq (? :one (? :any)) (? :term))
  (vector-rewrite ['(? :repeat (? :any) :min 0)]) ;=>>
  '(? :seq (? :repeat (? :any) :min 0) (? :term))
  ;; returns nil for map entries
  (vector-rewrite (first {:a 1})) ;=> nil

  ;;-------------------------------------------------------------------
  ;; set-rewrite - throws error for set patterns
  ;;-------------------------------------------------------------------
  ;; sets throw error with helpful message
  (try (set-rewrite #{1 2 3}) :no-error (catch Exception _ :threw)) ;=> :threw
  ;; returns nil for non-sets
  (set-rewrite [1 2 3]) ;=> nil
  (set-rewrite {:a 1}) ;=> nil

  ;;-------------------------------------------------------------------
  ;; matching-var-rewrite - transforms ?x, ?x?, ?x+, ?x* etc.
  ;;-------------------------------------------------------------------
  ;; simple ?x becomes variable binding
  (matching-var-rewrite '?x) ;=>>
  '(? :var x (? :any))
  ;; simple ?_ becomes wildcard
  (matching-var-rewrite '?_) ;=>>
  '(? :any)
  ;; ?x? optional with binding
  (matching-var-rewrite '?x?) ;=>>
  '(? :optional (? :var x (? :any)))
  ;; ?_? optional wildcard
  (matching-var-rewrite '?_?) ;=>>
  '(? :optional (? :any))
  ;; ?x+ one-or-more with binding
  (matching-var-rewrite '?x+) ;=>>
  '(? :repeat (? :any) :min 1 :max nil :as x)
  ;; ?x* zero-or-more with binding
  (matching-var-rewrite '?x*) ;=>>
  '(? :repeat (? :any) :min 0 :max nil :as x)
  ;; ?_* zero-or-more wildcard (no binding)
  (matching-var-rewrite '?_*) ;=>>
  '(? :repeat (? :any) :min 0 :max nil)
  ;; ?x+! greedy one-or-more
  (matching-var-rewrite '?x+!) ;=>>
  '(? :repeat (? :any) :min 1 :max nil :as x :greedy)
  ;; ?x*! greedy zero-or-more
  (matching-var-rewrite '?x*!) ;=>>
  '(? :repeat (? :any) :min 0 :max nil :as x :greedy)
  ;; returns nil for non-? symbols
  (matching-var-rewrite 'x) ;=> nil

  ;;-------------------------------------------------------------------
  ;; extended-var-rewrite - transforms (?x :opt1 val1 ...)
  ;;-------------------------------------------------------------------
  ;; (?a) with no options - same as ?a
  (extended-var-rewrite '(?a)) ;=>>
  '(? :var a (? :any))
  ;; (?_ ) wildcard with no options
  (extended-var-rewrite '(?_)) ;=>>
  '(? :any)
  ;; (?a :when pred) - with predicate
  (extended-var-rewrite (list '?a :when odd?)) ;=>>
  #(and (seq? %) (= '? (first %)) (= :var (second %)))
  ;; (?a :default val) - with default
  (extended-var-rewrite (list '?a :default 42)) ;=>>
  #(and (seq? %) (= '? (first %)) (= :var (second %)))
  ;; (?a :default val :when pred) - both options chained
  (extended-var-rewrite (list '?a :default 5 :when odd?)) ;=>>
  #(and (seq? %) (= '? (first %)) (= :var (second %)))
  ;; returns nil for non-matching-var lists
  (extended-var-rewrite '(foo bar)) ;=> nil
  ;; returns nil for invalid var names
  (extended-var-rewrite '(?foo*bar :when odd?)) ;=> nil

  ;;-------------------------------------------------------------------
  ;; extended-var-rewrite - pagination: :take and :skip
  ;;-------------------------------------------------------------------
  ;; (?x* :take N) - zero-or-more with limit, greedy
  (extended-var-rewrite '(?items* :take 3)) ;=>>
  '(? :repeat (? :any) :min 0 :max 3 :as items :greedy)
  ;; (?x+ :take N) - one-or-more with limit, greedy
  (extended-var-rewrite '(?items+ :take 5)) ;=>>
  '(? :repeat (? :any) :min 1 :max 5 :as items :greedy)
  ;; (?x*! :take N) - already greedy, still greedy with limit
  (extended-var-rewrite '(?items*! :take 3)) ;=>>
  '(? :repeat (? :any) :min 0 :max 3 :as items :greedy)
  ;; (?_* :take N) - wildcard zero-or-more with limit (no binding)
  (extended-var-rewrite '(?_* :take 3)) ;=>>
  '(? :repeat (? :any) :min 0 :max 3 :greedy)
  ;; (?x* :take N :when pred) - take with filter
  (extended-var-rewrite (list '?items* :take 3 :when seq)) ;=>>
  #(and (seq? %) (= '? (first %)) (= :-> (second %)))
  ;; (?_ :skip N) - skip exactly N elements
  (extended-var-rewrite '(?_ :skip 2)) ;=>>
  '(? :repeat (? :any) :min 2 :max 2)
  ;; (?_ :skip N) with additional option (unusual but possible)
  (extended-var-rewrite (list '?_ :skip 2 :when identity)) ;=>>
  #(and (seq? %) (= '? (first %)) (= :-> (second %)))

  ;;-------------------------------------------------------------------
  ;; core->matcher - compiles core patterns to matchers
  ;;-------------------------------------------------------------------
  ;; compiles :var pattern
  ((core->matcher '(? :var x (? :val 3))) (vmr 3)) ;=>>
  {:val 3 :vars {'x 3}}
  ;; compiles :map pattern (variadic syntax)
  ((core->matcher '(? :map :a (? :any))) (vmr {:a 5})) ;=>>
  {:val {:a 5}}
  ;; compiles :seq pattern (variadic syntax)
  ((core->matcher '(? :seq (? :one (? :any)) (? :term))) (vmr [5])) ;=>>
  {:val [5]}

  ;;-------------------------------------------------------------------
  ;; Two-phase compilation: rewrite then compile
  ;;-------------------------------------------------------------------
  ;; {} syntax sugar
  ((-> {:a '(? :val 3)}
       (rewrite-pattern [map-rewrite])
       core->matcher)
   (vmr {:a 3})) ;=>>
  {:val {:a 3}}
  ;; [] syntax sugar with auto-wrapping
  ((-> [3 4]
       (rewrite-pattern [vector-rewrite])
       core->matcher)
   (vmr [3 4])) ;=>>
  {:val [3 4]}
  ;; nested: map inside vector
  ((-> [{:a '(? :any)}]
       (rewrite-pattern [map-rewrite vector-rewrite])
       core->matcher)
   (vmr [{:a 5}])) ;=>>
  {:val [{:a 5}]}
  ;; nested: vector inside map
  ((-> {:items ['(? :any)]}
       (rewrite-pattern [map-rewrite vector-rewrite])
       core->matcher)
   (vmr {:items [1]})) ;=>>
  {:val {:items [1]}}
  ;; ?x variable binding
  ((-> {:name '?n}
       (rewrite-pattern [map-rewrite matching-var-rewrite])
       core->matcher)
   (vmr {:name "Alice"})) ;=>>
  {:val {:name "Alice"} :vars {'n "Alice"}}
  ;; ?_ wildcard (no binding)
  ((-> {:name '?_}
       (rewrite-pattern [map-rewrite matching-var-rewrite])
       core->matcher)
   (vmr {:name "Alice"})) ;=>>
  {:val {:name "Alice"} :vars {}}
  ;; extended var with :when option (must use list for actual fn)
  ((-> (list '?a :when odd?)
       (rewrite-pattern [extended-var-rewrite])
       core->matcher)
   (vmr 3)) ;=>>
  {:val 3 :vars {'a 3}}
  ;; extended var :when fails on even
  ((-> (list '?a :when odd?)
       (rewrite-pattern [extended-var-rewrite])
       core->matcher)
   (vmr 4)) ;=>>
  failure?
  ;; extended var with :default option
  ((-> (list '?a :default 42)
       (rewrite-pattern [extended-var-rewrite])
       core->matcher)
   (vmr nil)) ;=>>
  {:val 42 :vars {'a 42}}
  ;; extended var :default passes through non-nil
  ((-> (list '?a :default 42)
       (rewrite-pattern [extended-var-rewrite])
       core->matcher)
   (vmr 5)) ;=>>
  {:val 5 :vars {'a 5}}
  ;; extended var with both :default and :when
  ((-> (list '?a :default 5 :when odd?)
       (rewrite-pattern [extended-var-rewrite])
       core->matcher)
   (vmr nil)) ;=>>
  {:val 5 :vars {'a 5}})

;;=============================================================================
;; SECTION 4: Public API
;;=============================================================================

(def default-rewrite-rules
  "Default rewrite rules for pattern compilation.
   Order matters: more specific rules (extended-var) before general (matching-var)."
  [extended-var-rewrite
   matching-var-rewrite
   map-rewrite
   vector-rewrite
   list-rewrite
   set-rewrite])

(defn- wrap-with-schema-filter
  "Wrap matcher to filter :val by schema structure."
  [matcher s]
  (schema/wrap-with-schema-filter matcher s failure?))

(defn compile-pattern
  "Compile a pattern through two phases:
   1. Rewrite: Transform syntax sugar to core (? :type ...) patterns
   2. Compile: Build matcher functions from core patterns

   Options (optional second argument):
     :rules   - additional rewrite rules run BEFORE defaults (separate pass)
     :only    - use only these rules, ignoring defaults
     :schema  - schema to validate pattern against at compile time
     :resolve - custom symbol resolver (fn [sym] -> var-or-value)
     :eval-fn - custom form evaluator (fn [form] -> value)

   The :resolve and :eval-fn options enable:
   - Sandboxed evaluation for security (e.g., using SCI)
   - CLJS compatibility (provide SCI's resolve/eval)
   - Custom symbol lookup from a registry

   Schema format:
     Type keywords (structural validation):
       :map :seq :string :number :keyword :symbol :any

     Literal/Enum/Union/Optional:
       [:= value]    - literal value, e.g. [:= :active], [:= 42]
       #{:a :b :c}   - enum (set of allowed values)
       [:or s1 s2]   - union (one of several schemas)
       [:optional s] - nullable (nil allowed), type from inner schema

     Composite schemas:
       {:key schema ...}          - record map (specific fields)
       [:map-of key-type val-type] - dictionary map (homogeneous k/v types)
       [:element-schema]          - homogeneous seq (all elements same type)
       [:tuple schema1 ...]       - positional tuple (each position typed)

     Extensible: use register-schema-rule! to add custom schema types

   Examples:
     (compile-pattern '{:name ?n :age ?_})
     ; Returns a matcher that extracts :name into variable 'n

     ;; With schema validation
     (compile-pattern '{:name ?n} {:schema :map})
     (compile-pattern '{:name ?n :status ?s}
                      {:schema {:name :string :status [:= :active]}})
     (compile-pattern '[?x ?y] {:schema [:tuple :number :string]})
     (compile-pattern '{:x ?x :y ?y} {:schema [:map-of :keyword :number]})

     ;; With custom resolver/evaluator (e.g., SCI for security/CLJS)
     (compile-pattern '(? :pred odd?)
                      {:resolve sci/resolve :eval-fn sci/eval})"
  ([ptn]
   (compile-pattern ptn nil))
  ([ptn {:keys [rules only schema resolve eval-fn]}]
   (let [rewritten (cond
                     only (rewrite-pattern ptn only)
                     rules (-> ptn
                               (rewrite-pattern rules)  ; custom rules first
                               (rewrite-pattern default-rewrite-rules))  ; then defaults
                     :else (rewrite-pattern ptn default-rewrite-rules))]
     ;; Validate against schema before compilation (if provided)
     (when schema
       (validate-pattern-schema! rewritten schema))
     ;; Compile with custom resolver/evaluator if provided
     (binding [*resolve-sym* (or resolve *resolve-sym*)
               *eval-form* (or eval-fn *eval-form*)]
       (cond-> (core->matcher rewritten)
         ;; Filter output to schema's valid-keys for record schemas
         schema (wrap-with-schema-filter schema))))))

;;-----------------------------------------------------------------------------
;; Rule: Pattern → Template Transformation
;;-----------------------------------------------------------------------------

(defn substitute-template
  "Replace ?vars in template with bound values from vars map.
   Handles quantifier suffixes (?x*, ?x+, ?x?) by stripping them.
   Variables not found in vars are left unchanged."
  [template vars]
  (walk/postwalk
   (fn [x]
     (if (variable? x)
       (let [nm (name x)
             ;; Strip ? prefix and any quantifier suffix
             base (if-let [[_ b] (re-matches #"\?([^\s\?\+\*\!]+)[\?\+\*]?\!?" nm)]
                    b
                    (subs nm 1))
             var-sym (symbol base)]
         (get vars var-sym x))
       x))
   template))

(defmacro rule
  "Create a transformation rule: pattern → template.

   Returns a function that:
   - On match: returns template with ?vars substituted
   - On no match: returns nil

   Can be used for:
   1. Runtime data transformation
   2. Compile-time pattern rewriting (via :rules option in compile-pattern)

   Examples:
     ;; Algebraic simplification
     (def double-to-add (rule (* 2 ?x) (+ ?x ?x)))
     (double-to-add '(* 2 5))  ;=> (+ 5 5)
     (double-to-add '(* 3 5))  ;=> nil

     ;; Sequence transformation
     (def rotate (rule [?first ?rest*] [?rest* ?first]))
     (rotate '(a b c))  ;=> ((b c) a)  ; note: ?rest* is a list

     ;; Custom pattern syntax (compile-time rewriting)
     (def not-nil (rule (not-nil ?x) (?x :when some?)))
     (compile-pattern '{:name (not-nil ?n)} {:rules [not-nil]})"
  [pattern template]
  `(let [matcher# (compile-pattern '~pattern)]
     (fn [data#]
       (let [result# (matcher# (vmr data#))]
         (when-not (failure? result#)
           (substitute-template '~template (:vars result#)))))))

(defn apply-rules
  "Apply rules recursively throughout a data structure (bottom-up).
   Each node is transformed by the first matching rule.
   Returns the fully transformed structure."
  [rules data]
  (walk/postwalk
   (fn [x]
     (or (some #(% x) rules) x))
   data))

(defmacro match-fn
  "Create a pattern-matching function.
   Returns body result on match, MatchFailure on failure.

   Pattern syntax:
     ?x       - Bind to symbol x
     ?_       - Wildcard (no binding)
     ?x?      - Optional (0-1)
     ?x* ?x+  - Zero/one-or-more (lazy)
     ?x*! ?x+!- Zero/one-or-more (greedy)
     {}       - Map pattern
     []       - Sequence pattern

   Pagination (sequence patterns):
     (?x* :take N)  - Take up to N elements (greedy)
     (?_ :skip N)   - Skip exactly N elements
     NOTE: Add ?_* at end to consume remaining elements

   Variable options:
     (?x :when pred)    - Constrained match
     (?x :default val)  - Default on failure
     NOTE: :when on quantified vars (?x* :when pred) filters individual
           elements during collection, not the final collected result.
           To validate the collection, use body: (when (pred ?x) ?x)

   Compile options (optional third argument):
     :rules   - custom rewrite rules (prepended to defaults)
     :only    - use only these rules, ignoring defaults
     :schema  - schema for compile-time validation
     :resolve - custom symbol resolver (fn [sym] -> value)
     :eval-fn - custom form evaluator (fn [form] -> value)

   Special binding: $ is bound to the matched/transformed value.

   Examples:
     ((match-fn {:a ?a :b ?b} (+ ?a ?b)) {:a 1 :b 2})  ;=> 3
     ((match-fn [?first ?rest*] ?rest) [1 2 3])        ;=> (2 3)
     ((match-fn {:a ?x} (assoc $ :sum ?x)) {:a 1 :b 2});=> {:a 1 :b 2 :sum 1}
     ((match-fn [(?_ :skip 2) (?page* :take 3) ?_*] ?page) [1 2 3 4 5 6 7]) ;=> [3 4 5]

     ;; With schema validation
     ((match-fn {:name ?n} ?n {:schema {:name :string}}) {:name \"alice\"}) ;=> \"alice\"

     ;; With custom rules
     (let [not-nil (rule (not-nil ?x) (?x :when some?))]
       ((match-fn {:name (not-nil ?n)} ?n {:rules [not-nil]}) {:name \"bob\"})) ;=> \"bob\""
  ([pattern body] `(match-fn ~pattern ~body {}))
  ([pattern body opts]
   (let [;; Collect all variable bindings from pattern
         vars (atom #{})
         _ (walk/postwalk
            (fn [x]
              (when (variable? x)
                (let [nm (name x)
                      ;; Strip prefix ? and any suffix quantifiers
                      base (if-let [[_ b] (re-matches #"\?([^\s\?\+\*\!]+)[\?\+\*]?\!?" nm)]
                             b
                             (subs nm 1))]
                  (when (and (not= "_" base)
                             (not (re-find forbidden-var-chars base)))
                    (swap! vars conj (symbol base)))))
              x)
            pattern)
         var-syms @vars
         ;; Create let bindings: ?x -> (get vars 'x)
         bindings (vec (mapcat (fn [s] [(symbol (str "?" s)) `(get ~'vars '~s)]) var-syms))]
     `(let [matcher# (compile-pattern '~pattern ~opts)]
        (fn [data#]
          (let [result# (matcher# (vmr data#))]
            (if (failure? result#)
              result#
              (let [~'$ data#
                    ~'vars (:vars result#)
                    ~@bindings]
                ~body))))))))

;;-----------------------------------------------------------------------------
;; Built-in Schema Rules (match-fn based)
;;-----------------------------------------------------------------------------
;; Wrap match-fn rules to return nil instead of MatchFailure on non-match.

#?(:clj
   (defn- schema-rule
     "Wrap match-fn to return nil on non-match (for schema rules)."
     [mf]
     (fn [s] (let [r (mf s)] (when-not (failure? r) r)))))

#?(:clj
   (do
     (register-schema-rule! (schema-rule (match-fn [:= ?v] {:type (schema/infer-value-type ?v)})))
     (register-schema-rule! (schema-rule (match-fn [:map-of ?_ ?v] {:type :map :child-schema (constantly ?v)})))
     (register-schema-rule! (schema-rule (match-fn [:or ?s*] {:type (let [ts (set (map infer-schema-type ?s))]
                                                                      (if (= 1 (count ts)) (first ts) :any))})))
     (register-schema-rule! (schema-rule (match-fn [:optional ?s] (get-schema-info ?s))))
     (register-schema-rule! (schema-rule (match-fn [:tuple ?t*] {:type :seq :child-schema #(nth ?t % :any)})))))

^:rct/test
(comment
  ;;-------------------------------------------------------------------
  ;; compile-pattern - two-phase compilation
  ;;-------------------------------------------------------------------
  ;; basic map pattern with variable binding
  ((compile-pattern '{:name ?n}) (vmr {:name "Alice"})) ;=>>
  {:val {:name "Alice"} :vars {'n "Alice"}}
  ;; wildcard ?_ doesn't bind
  ((compile-pattern '{:name ?n :age ?_}) (vmr {:name "Bob" :age 30})) ;=>>
  {:val {:name "Bob" :age 30} :vars {'n "Bob"}}
  ;; vector pattern with variable
  ((compile-pattern '[?first ?rest*]) (vmr [1 2 3])) ;=>>
  {:val [1 2 3] :vars {'first 1 'rest [2 3]}}
  ;; nested patterns
  ((compile-pattern '{:user {:name ?n}}) (vmr {:user {:name "Eve"}})) ;=>>
  {:vars {'n "Eve"}}
  ;; map unification: same var with same value succeeds
  ((compile-pattern '{:a ?x :b ?x}) (vmr {:a 1 :b 1})) ;=>>
  {:vars {'x 1}}
  ;; map unification: same var with different values fails
  ((compile-pattern '{:a ?x :b ?x}) (vmr {:a 1 :b 2})) ;=>>
  failure?

  ;;-------------------------------------------------------------------
  ;; match-fn macro - pattern matching function
  ;;-------------------------------------------------------------------
  ;; basic binding extraction
  ((match-fn {:a ?a :b ?b} (+ ?a ?b)) {:a 1 :b 2}) ;=> 3
  ;; sequence pattern
  ((match-fn [?first ?rest*] {:first ?first :rest ?rest}) [1 2 3]) ;=>>
  {:first 1 :rest [2 3]}
  ;; access to matched value via $
  ((match-fn {:a ?x} (assoc $ :sum ?x)) {:a 1 :b 2}) ;=>>
  {:a 1 :b 2 :sum 1}
  ;; failure returns MatchFailure
  ((match-fn {:a ?a} ?a) "not a map") ;=>> failure?
  ;; wildcard with body
  ((match-fn {:name ?_} :ok) {:name "ignored"}) ;=> :ok

  ;;-------------------------------------------------------------------
  ;; substitute-template - variable substitution in templates
  ;;-------------------------------------------------------------------
  ;; replaces ?vars with values from vars map
  (substitute-template '(+ ?x ?y) {'x 1 'y 2}) ;=>> '(+ 1 2)
  ;; leaves unbound vars unchanged
  (substitute-template '(+ ?x ?z) {'x 1}) ;=>> '(+ 1 ?z)
  ;; works with nested structures
  (substitute-template '{:a ?x :b [?y ?z]} {'x 1 'y 2 'z 3}) ;=>>
  {:a 1 :b [2 3]}

  ;;-------------------------------------------------------------------
  ;; rule macro - pattern → template transformation
  ;;-------------------------------------------------------------------
  ;; basic rule: algebraic simplification
  ((rule (* 2 ?x) (+ ?x ?x)) '(* 2 5)) ;=>> '(+ 5 5)
  ;; rule returns nil on no match
  ((rule (* 2 ?x) (+ ?x ?x)) '(* 3 5)) ;=> nil
  ;; rule with map patterns
  ((rule {:type :add :a ?a :b ?b} (+ ?a ?b)) {:type :add :a 1 :b 2}) ;=>>
  '(+ 1 2)
  ;; rule with sequence patterns
  ((rule [?first ?rest*] [?rest* ?first]) [1 2 3]) ;=>>
  #(and (vector? %) (= (first %) [2 3]))

  ;;-------------------------------------------------------------------
  ;; apply-rules - recursive tree transformation
  ;;-------------------------------------------------------------------
  ;; applies rules bottom-up throughout tree
  (apply-rules [(rule (* 2 ?x) (+ ?x ?x))]
               '(foo (* 2 3) bar)) ;=>>
  '(foo (+ 3 3) bar)
  ;; multiple rules, first match wins
  (apply-rules [(rule (* 2 ?x) (+ ?x ?x))
                (rule (+ 0 ?x) ?x)]
               '(+ 0 (* 2 y))) ;=>>
  '(+ y y)
  ;; nested application
  (apply-rules [(rule (* 2 ?x) (+ ?x ?x))]
               '(* 2 (* 2 z))) ;=>>
  '(+ (+ z z) (+ z z))

  ;;-------------------------------------------------------------------
  ;; compile-pattern with custom rules (function-based rewrite rules)
  ;;-------------------------------------------------------------------
  ;; custom rewrite rule as a function - use actual fn, not symbol
  (let [not-nil-rewrite (fn [x]
                          (when (and (seq? x) (= 'not-nil (first x)))
                            (let [var (second x)]
                              (list var :when some?))))]  ; some? not 'some?
    ((compile-pattern '(not-nil ?n) {:rules [not-nil-rewrite]}) (vmr "hello"))) ;=>>
  {:vars {'n "hello"}}
  ;; custom rule fails when predicate fails
  (let [not-nil-rewrite (fn [x]
                          (when (and (seq? x) (= 'not-nil (first x)))
                            (let [var (second x)]
                              (list var :when some?))))]
    ((compile-pattern '(not-nil ?n) {:rules [not-nil-rewrite]}) (vmr nil))) ;=>>
  failure?
  ;; :only option uses only specified rules (no defaults)
  (let [identity-rule (fn [x] (when (= x 'foo) '(? :val 42)))]
    ((compile-pattern 'foo {:only [identity-rule]}) (vmr 42))) ;=>>
  {:val 42}

  ;;-------------------------------------------------------------------
  ;; Pagination: :take and :skip
  ;;-------------------------------------------------------------------
  ;; Note: Vector patterns add (? :term) at end, so use ?_* to consume rest
  ;; (?items* :take 3) - take first 3 elements, ignore rest
  ((compile-pattern '[(?items* :take 3) ?_*]) (vmr [1 2 3 4 5])) ;=>>
  {:vars {'items [1 2 3]}}
  ;; (?items* :take 3) - works when fewer elements available
  ((compile-pattern '[(?items* :take 3)]) (vmr [1 2])) ;=>>
  {:vars {'items [1 2]}}
  ;; (?items+ :take 3) - one-or-more with limit
  ((compile-pattern '[(?items+ :take 3) ?_*]) (vmr [1 2 3 4 5])) ;=>>
  {:vars {'items [1 2 3]}}
  ;; (?items+ :take 3) - fails on empty (needs at least 1)
  ((compile-pattern '[(?items+ :take 3)]) (vmr [])) ;=>> failure?
  ;; (?_ :skip 2) - skip first 2 elements
  ((compile-pattern '[(?_ :skip 2) ?rest*]) (vmr [1 2 3 4 5])) ;=>>
  {:vars {'rest [3 4 5]}}
  ;; Pagination: skip 2, take 3, ignore rest
  ((compile-pattern '[(?_ :skip 2) (?page* :take 3) ?_*]) (vmr [1 2 3 4 5 6 7 8 9 10])) ;=>>
  {:vars {'page [3 4 5]}}
  ;; Pagination: skip 2, take 3, capture rest
  ((compile-pattern '[(?_ :skip 2) (?page* :take 3) ?rest*]) (vmr [1 2 3 4 5 6 7 8 9 10])) ;=>>
  {:vars {'page [3 4 5] 'rest [6 7 8 9 10]}}
  ;; Note: :when on quantified vars applies to individual elements, not the collection
  ;; To filter the collected result, use match-fn with post-processing

  ;;-------------------------------------------------------------------
  ;; Schema validation: compile-time type checking
  ;;-------------------------------------------------------------------
  ;; Valid: map pattern on :map schema
  (compile-pattern '{:a ?x} {:schema :map}) ;=>> fn?

  ;; Valid: seq pattern on :seq schema
  (compile-pattern '[?x ?y] {:schema :seq}) ;=>> fn?

  ;; Valid: variable pattern on any schema (vars match any type)
  (compile-pattern '?x {:schema :map}) ;=>> fn?
  (compile-pattern '?x {:schema :string}) ;=>> fn?

  ;; Valid: :default and :when chain patterns on scalar schema
  (compile-pattern '{:name (?n :default "anon")} {:schema {:name :string}}) ;=>> fn?
  (compile-pattern '{:age (?a :when pos?)} {:schema {:age :number}}) ;=>> fn?

  ;; Invalid: key not in schema throws even with :default
  (try
    (compile-pattern '{:debug (?d :default false)} {:schema {:version :string}})
    (catch clojure.lang.ExceptionInfo e
      (:key (ex-data e))))
  ;=> :debug

  ;; Invalid: seq pattern on :map schema - throws
  (try
    (compile-pattern '[?x] {:schema :map})
    (catch clojure.lang.ExceptionInfo e
      (:pattern-type (ex-data e))))
  ;=> :seq

  ;; Invalid: map pattern on :seq schema - throws
  (try
    (compile-pattern '{:a ?x} {:schema :seq})
    (catch clojure.lang.ExceptionInfo e
      (:pattern-type (ex-data e))))
  ;=> :map

  ;; Nested schema: map with field types
  (compile-pattern '{:items [?x ?y]} {:schema {:items [:any]}}) ;=>> fn?

  ;; Nested schema: field is seq, pattern correctly uses seq
  (compile-pattern '{:name ?n :tags [?first ?rest*]}
                   {:schema {:name :string :tags [:string]}}) ;=>> fn?

  ;; Invalid nested: map pattern where seq expected
  (try
    (compile-pattern '{:items {:x ?x}} {:schema {:items [:any]}})
    (catch clojure.lang.ExceptionInfo e
      (:schema-type (ex-data e))))
  ;=> :seq

  ;; Invalid nested: seq pattern where map expected
  (try
    (compile-pattern '{:user [?a ?b]} {:schema {:user {:name :string}}})
    (catch clojure.lang.ExceptionInfo e
      (:schema-type (ex-data e))))
  ;=> :map

  ;; Schema without option still compiles normally
  ((compile-pattern '{:a ?x}) (vmr {:a 42})) ;=>> {:vars {'x 42}}

  ;; Schema :any allows everything
  (compile-pattern '{:a ?x} {:schema :any}) ;=>> fn?
  (compile-pattern '[?x] {:schema :any}) ;=>> fn?

  ;;-------------------------------------------------------------------
  ;; Schema output filtering: record schemas filter :val to valid-keys
  ;;-------------------------------------------------------------------
  ;; Empty {} pattern with record schema - filters to schema keys only
  ((compile-pattern '{} {:schema {:name :string :age :number}})
   (vmr {:name "Alice" :age 30 :password "secret"})) ;=>>
  {:val {:name "Alice" :age 30}}

  ;; Pattern with explicit keys - $ also filtered to schema keys
  ((compile-pattern '{:name ?n} {:schema {:name :string :age :number}})
   (vmr {:name "Alice" :age 30 :password "secret"})) ;=>>
  {:val {:name "Alice" :age 30} :vars {'n "Alice"}}

  ;; Generic :map schema - no filtering (any keys allowed)
  ((compile-pattern '{} {:schema :map})
   (vmr {:name "Alice" :password "secret"})) ;=>>
  {:val {:name "Alice" :password "secret"}}

  ;; [:map-of k v] schema - no filtering (dictionary pattern)
  ((compile-pattern '{} {:schema [:map-of :keyword :any]})
   (vmr {:name "Alice" :password "secret"})) ;=>>
  {:val {:name "Alice" :password "secret"}}

  ;; No schema - no filtering (backward compatible)
  ((compile-pattern '{})
   (vmr {:name "Alice" :password "secret"})) ;=>>
  {:val {:name "Alice" :password "secret"}}

  ;; Nested record schema - filters recursively at all levels
  ((compile-pattern '{} {:schema {:user {:name :string :age :number}}})
   (vmr {:user {:name "Alice" :age 30 :password "secret"} :extra "removed"})) ;=>>
  {:val {:user {:name "Alice" :age 30}}}

  ;; Sequence of records - filters each element
  ((compile-pattern '{} {:schema {:users [{:name :string}]}})
   (vmr {:users [{:name "Alice" :password "x"} {:name "Bob" :ssn "y"}]})) ;=>>
  {:val {:users [{:name "Alice"} {:name "Bob"}]}}

  ;; Deeply nested: records inside sequences inside records
  ((compile-pattern '{} {:schema {:dept {:employees [{:name :string}]}}})
   (vmr {:dept {:employees [{:name "Alice" :salary 100}] :budget 999} :secret "x"})) ;=>>
  {:val {:dept {:employees [{:name "Alice"}]}}}

  ;;-------------------------------------------------------------------
  ;; Tuple schema: [:tuple type1 type2 ...]
  ;;-------------------------------------------------------------------
  ;; Valid: pattern matches tuple positionally
  (compile-pattern '[?x ?y] {:schema [:tuple :number :string]}) ;=>> fn?

  ;; Tuple in nested schema
  ((compile-pattern '{:point [?x ?y]} {:schema {:point [:tuple :number :number]}})
   (vmr {:point [10 20]})) ;=>> {:vars {'x 10 'y 20}}

  ;; Invalid: map pattern where tuple expects :number
  (try
    (compile-pattern '[{:a ?a} ?y] {:schema [:tuple :number :string]})
    (catch clojure.lang.ExceptionInfo e
      (:schema-type (ex-data e))))
  ;=> :number

  ;;-------------------------------------------------------------------
  ;; Literal schema: [:= value]
  ;;-------------------------------------------------------------------
  ;; Literal keyword - infers as :keyword
  (infer-schema-type [:= :active]) ;=> :keyword

  ;; Literal string - infers as :string
  (infer-schema-type [:= "hello"]) ;=> :string

  ;; Literal number - infers as :number
  (infer-schema-type [:= 42]) ;=> :number

  ;; Literal map - infers as :map
  (infer-schema-type [:= {:type :user}]) ;=> :map

  ;; Valid: var pattern for literal keyword
  (compile-pattern '{:status ?s} {:schema {:status [:= :active]}}) ;=>> fn?

  ;; Invalid: map pattern where literal keyword expected
  (try
    (compile-pattern '{:status {:x ?x}} {:schema {:status [:= :active]}})
    (catch clojure.lang.ExceptionInfo e
      (:schema-type (ex-data e))))
  ;=> :keyword

  ;; Mixed: types and literals together
  ((compile-pattern '{:name ?n :status ?s}
                    {:schema {:name :string :status [:= :active]}})
   (vmr {:name "Alice" :status :active})) ;=>> {:vars {'n "Alice" 's :active}}

  ;;-------------------------------------------------------------------
  ;; Dictionary schema: [:map-of key-type value-type]
  ;;-------------------------------------------------------------------
  ;; Dictionary schema infers as :map
  (infer-schema-type [:map-of :keyword :number]) ;=> :map

  ;; Valid: map pattern with dictionary schema
  ((compile-pattern '{:x ?x :y ?y} {:schema [:map-of :keyword :number]})
   (vmr {:x 10 :y 20})) ;=>> {:vars {'x 10 'y 20}}

  ;; Invalid: nested map where number expected
  (try
    (compile-pattern '{:x {:a ?a}} {:schema [:map-of :keyword :number]})
    (catch clojure.lang.ExceptionInfo e
      (:schema-type (ex-data e))))
  ;=> :number

  ;; Nested dictionary in record
  (compile-pattern '{:data {:a ?a}} {:schema {:data [:map-of :keyword :any]}}) ;=>> fn?

  ;;-------------------------------------------------------------------
  ;; Enum schema: #{value1 value2 ...}
  ;;-------------------------------------------------------------------
  ;; Keyword enum infers as :keyword
  (infer-schema-type #{:active :inactive}) ;=> :keyword

  ;; String enum infers as :string
  (infer-schema-type #{"foo" "bar"}) ;=> :string

  ;; Number enum infers as :number
  (infer-schema-type #{1 2 3}) ;=> :number

  ;; Mixed enum infers as :any
  (infer-schema-type #{:a 1 "x"}) ;=> :any

  ;; Enum in record field
  ((compile-pattern '{:status ?s} {:schema {:status #{:active :inactive}}})
   (vmr {:status :active})) ;=>> {:vars {'s :active}}

  ;; Invalid: map pattern for keyword enum
  (try
    (compile-pattern '{:status {:x ?x}} {:schema {:status #{:active :inactive}}})
    (catch clojure.lang.ExceptionInfo e
      (:schema-type (ex-data e))))
  ;=> :keyword

  ;;-------------------------------------------------------------------
  ;; Union schema: [:or schema1 schema2 ...]
  ;;-------------------------------------------------------------------
  ;; Same types -> that type
  (infer-schema-type [:or :string :string]) ;=> :string

  ;; Different primitive types -> :any
  (infer-schema-type [:or :string :number]) ;=> :any

  ;; Both maps -> :map
  (infer-schema-type [:or {:a :string} {:b :number}]) ;=> :map

  ;; Union of maps allows map pattern
  (compile-pattern '{:x ?x} {:schema [:or {:x :string} {:x :number}]}) ;=>> fn?

  ;; Union in record field
  (compile-pattern '{:value ?v} {:schema {:value [:or :string :number]}}) ;=>> fn?

  ;;-------------------------------------------------------------------
  ;; Optional schema: [:optional inner-schema]
  ;;-------------------------------------------------------------------
  ;; Optional unwraps to inner type
  (infer-schema-type [:optional :string]) ;=> :string
  (infer-schema-type [:optional {:a :number}]) ;=> :map
  (infer-schema-type [:optional #{:a :b}]) ;=> :keyword

  ;; Optional in record field
  (compile-pattern '{:name ?n :nick ?nick}
                   {:schema {:name :string :nick [:optional :string]}}) ;=>> fn?

  ;; Invalid: map pattern for optional string
  (try
    (compile-pattern '{:nick {:x ?x}} {:schema {:nick [:optional :string]}})
    (catch clojure.lang.ExceptionInfo e
      (:schema-type (ex-data e))))
  ;=> :string

  ;;-------------------------------------------------------------------
  ;; Indexed lookup: map keys that are not keywords
  ;;-------------------------------------------------------------------
  ;; Map keys that are maps themselves should be used as literal lookup keys,
  ;; not rewritten as patterns. This is essential for indexed lookup patterns
  ;; like {:posts/history {{:post/id 1} ?versions}}

  ;; Map with map key - key should be treated as literal, not a pattern
  ((compile-pattern '{{:id 1} ?result}) (vmr {{:id 1} {:name "Alice"}})) ;=>>
  {:val {{:id 1} {:name "Alice"}} :vars {'result {:name "Alice"}}}

  ;; Map with vector key - key should be treated as literal
  ((compile-pattern '{[1 2] ?result}) (vmr {[1 2] "found"})) ;=>>
  {:val {[1 2] "found"} :vars {'result "found"}}

  ;; Nested indexed lookup
  ((compile-pattern '{:data {{:id 1} ?x}}) (vmr {:data {{:id 1} 42}})) ;=>>
  {:vars {'x 42}}

  ;;-------------------------------------------------------------------
  ;; Indexed lookup with :ilookup schema annotation (Malli)
  ;;-------------------------------------------------------------------
  ;; Collections that support ILookup (like database-backed collections) can
  ;; be queried with map keys like {:id 1}. The :ilookup Malli property
  ;; enables validation of indexed lookup patterns.
  ;; Syntax: [:vector {:ilookup true} element-schema]

  ;; Require malli for these tests
  (require '[malli.core :as m])
  (require '[sg.flybot.pullable.malli])

  ;; Without :ilookup - indexed lookup pattern on seq schema fails
  (try
    (compile-pattern '{:users {{:id 1} ?u}}
                     {:schema (m/schema [:map [:users [:vector [:map [:id :int] [:name :string]]]]])})
    :should-have-thrown
    (catch clojure.lang.ExceptionInfo e
      (:pattern-type (ex-data e))))
  ;=> :map

  ;; With :ilookup true - indexed lookup pattern compiles
  (compile-pattern '{:users {{:id 1} ?u}}
                   {:schema (m/schema [:map [:users [:vector {:ilookup true}
                                                     [:map [:id :int] [:name :string]]]]])})
  ;=>> fn?

  ;; Invalid field in indexed lookup value pattern - reports offending key
  (try
    (compile-pattern '{:users {{:id 1} {:invalid ?x}}}
                     {:schema (m/schema [:map [:users [:vector {:ilookup true}
                                                       [:map [:id :int] [:name :string]]]]])})
    :should-have-thrown
    (catch clojure.lang.ExceptionInfo e
      (:key (ex-data e))))
  ;=> :invalid

  ;; Valid field in indexed lookup value pattern
  (compile-pattern '{:users {{:id 1} {:name ?n}}}
                   {:schema (m/schema [:map [:users [:vector {:ilookup true}
                                                     [:map [:id :int] [:name :string]]]]])})
  ;=>> fn?

  ;; Deeply nested: indexed lookup inside indexed lookup
  (compile-pattern '{:depts {{:id "eng"} {:members {{:id 1} ?member}}}}
                   {:schema (m/schema [:map [:depts [:vector {:ilookup true}
                                                     [:map
                                                      [:id :string]
                                                      [:members [:vector {:ilookup true}
                                                                 [:map [:id :int] [:name :string]]]]]]]])}))
  ;=>> fn?)

