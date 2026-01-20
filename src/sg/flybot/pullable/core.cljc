(ns sg.flybot.pullable.core
  "Core pattern matching engine.

   This namespace provides the pattern matching implementation:
   - Match result types (ValMatchResult, MatchFailure)
   - Matcher primitives (mpred, mval, mmap, mseq, mor, etc.)
   - Pattern DSL and compilation (core->matcher, defmatcher)"
  (:require
   [clojure.pprint :refer [cl-format]]
   [clojure.walk :as walk]
   [clojure.zip :as zip]
   [sg.flybot.pullable.util :refer [vars->]]))

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
  "Merge variables from `other` into `mr`"
  [mr other]
  (if-let [vo (:vars other)]
    (vmr (:val mr) (merge (:vars mr) vo))
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
  ;; merge-vars - merge variable bindings
  ;;-------------------------------------------------------------------
  ;; merges vars from other into mr
  (merge-vars (vmr 1 {'a 1}) (vmr 2 {'b 2})) ;=>> {:val 1 :vars {'a 1 'b 2}}
  ;; handles nil vars gracefully
  (merge-vars (vmr 1 {'a 1}) (vmr 2)) ;=>> {:val 1 :vars {'a 1}}

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

(def ^:private matcher-specs* (atom {}))

(defmacro defmatcher
  "Define a matcher type specification.
   type - keyword like :pred, :val, etc.
   args - human-readable argument format string
   parse - matcher that validates and extracts arguments
   make - a (fn [vars] ...) that creates the matcher from parsed vars
   opts - optional map with :subseq? true for subsequence matchers"
  ([type args parse make]
   `(defmatcher ~type ~args ~parse ~make {}))
  ([type args parse make opts]
   `(swap! matcher-specs* assoc ~type
           (merge {:args ~args
                   :parse ~parse
                   :make ~make}
                  ~opts))))

(defmatcher :pred "(? :pred <fn>)"
  (mzone (mvar 'f wildcard))
  (vars-> [f] (mpred f)))

(defmatcher :val "(? :val <value>)"
  (mzone (mvar 'val wildcard))
  (vars-> [val] (mval val)))

(defmatcher :any "(? :any)"
  mterm  ;; no args expected
  (vars-> [] wildcard))

(defmatcher :map "(? :map <key> <pattern>...)"
  ;; Collect key-pattern pairs as flat variadic args
  (mzcollect (constantly true) 'pairs 0)
  (vars-> [pairs]
          (when-not (and (even? (count pairs))
                         (every? keyword? (take-nth 2 pairs))
                         (every? matcher-type (take-nth 2 (rest pairs))))
            (throw (ex-info "Invalid :map args: expected alternating keywords and matchers"
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
   Each rule is a fn: pattern -> core-pattern or nil"
  [ptn rules]
  (walk/postwalk
   (fn [x]
     (or (some #(% x) rules) x))
   ptn))

(defn map-rewrite
  "Rewrite map literal {} to (? :map ...) core pattern.
   Returns nil if not applicable."
  [x]
  (when (and (map? x) (not (record? x)))
    (apply list '? :map (mapcat identity x))))

(defn- subseq-type?
  "Check if a matcher type keyword has :subseq? true in its spec"
  [t]
  (some-> @matcher-specs* (get t) :subseq?))

(defn- subseq-pattern?
  "Check if x is a subseq core pattern like (? :one ...), (? :repeat ...), etc."
  [x]
  (and (seq? x)
       (= '? (first x))
       (subseq-type? (second x))))

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
   Returns nil if not applicable."
  [x]
  (when (symbol? x)
    (let [nm (name x)]
      (when (= \? (first nm))
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

;; Built-in option: :when - adds a predicate check
(register-var-option! :when
                      (fn [pred]
                        (list '? :pred pred)))

;; Built-in option: :default - provides default for nil values
(register-var-option! :default
                      (fn [default-val]
                        (list '? :sub (fn [x] (if (nil? x) default-val x)))))

(defn extended-var-rewrite
  "Rewrite (?var :opt1 val1 :opt2 val2 ...) to pattern.
   Options are chained in order: base -> opt1 -> opt2 -> ...
   Returns nil if not applicable."
  [form]
  (when (and (seq? form)
             (symbol? (first form))
             (= \? (first (name (first form)))))
    (let [[var-sym & opts] form
          var-name (subs (name var-sym) 1)]
      (when-not (re-find forbidden-var-chars var-name)
        (let [;; Parse options as keyword-value pairs
              opt-pairs (partition 2 opts)
              ;; Build chain of option patterns
              opt-patterns (for [[k v] opt-pairs
                                 :let [handler (get @var-option-handlers k)]
                                 :when handler]
                             (handler v))
              ;; Base pattern
              base '(? :any)
              ;; Chain: base -> opt1 -> opt2 -> ...
              inner (if (seq opt-patterns)
                      (apply list '? :-> base opt-patterns)
                      base)]
          (if (= "_" var-name)
            inner
            (list '? :var (symbol var-name) inner)))))))

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
  ;; basic map rewrite
  (map-rewrite {:a '(? :any)}) ;=>>
  '(? :map :a (? :any))
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
  ;; (?a :when pred) - with predicate (note: pred is symbol in quoted form)
  (extended-var-rewrite '(?a :when odd?)) ;=>>
  '(? :var a (? :-> (? :any) (? :pred odd?)))
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
   vector-rewrite])

(defn compile-pattern
  "Compile a pattern through two phases:
   1. Rewrite: Transform syntax sugar to core (? :type ...) patterns
   2. Compile: Build matcher functions from core patterns

   Example:
     (compile-pattern '{:name ?n :age ?_})
     ; Returns a matcher that extracts :name into variable 'n"
  [ptn]
  (-> ptn
      (rewrite-pattern default-rewrite-rules)
      core->matcher))

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

   Special binding: $ is bound to the matched/transformed value.

   Examples:
     ((match-fn {:a ?a :b ?b} (+ ?a ?b)) {:a 1 :b 2})  ;=> 3
     ((match-fn [?first ?rest*] ?rest) [1 2 3])        ;=> (2 3)
     ((match-fn {:a ?x} (assoc $ :sum ?x)) {:a 1 :b 2});=> {:a 1 :b 2 :sum 1}"
  [pattern body]
  (let [;; Collect all variable bindings from pattern
        vars (atom #{})
        _ (walk/postwalk
           (fn [x]
             (when (and (symbol? x) (= \? (first (name x))))
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
    `(let [matcher# (compile-pattern '~pattern)]
       (fn [data#]
         (let [result# (matcher# (vmr data#))]
           (if (failure? result#)
             result#
             (let [~'$ (:val result#)
                   ~'vars (:vars result#)
                   ~@bindings]
               ~body)))))))

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
  ((match-fn {:name ?_} :ok) {:name "ignored"})) ;=> :ok)

