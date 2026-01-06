(ns sg.flybot.pullable.core
  (:require
   [clojure.walk :as walk]
   [clojure.zip :as zip]))

;;# Matcher and matcher result
;;
;; A matcher is a function takes a match result (mr) and returns a mr, or nil if not matching.
;;
;; A matcher result is a map contains `:val` as it value, `:vars` is a map contains
;; matching variable values.

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
   (vmr val nil))
  ([val vars]
   (ValMatchResult. val vars)))

(defn merge-vars
  [mr other]
  (if-let [vo (:vars other)]
    (vmr (:val mr) (merge (:vars mr) vo))
    mr))

^:rct/test
(comment
  (vapply (vmr 3) inc) ;=>>
  {:val 4}
  (-bind (vmr 3) 'a 4) ;=>> {:vars {'a 4}}
  (fit (vmr 3) odd?) ;=>>
  {:val 3})

;;## Implementation of matchers 
;;A matcher is a function take a mr and returns a mr, if not match, returns nil

(defn matcher [mt f & {:as other}]
  (with-meta f (merge other {::matcher-type mt})))

(defn matcher-type [x] (some-> x meta ::matcher-type))

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

(def wildcard (matcher :wildcard identity))

(defn msub
  "substitute mr's `:val` with `f` applies to it"
  [f]
  (matcher :sub #(vapply % f)))

(defn mf
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
  ;;mpred succeed when `pred` success
  (map (mvar 'a (mpred even?)) [(vmr 4) (vmr 3)]) ;=>>
  [{:val 4 :vars {'a 4}} failure?]
  ((mval 3) (vmr 3)) ;=>>
  {:val 3}
  ;;msub operates on `val` of a mr
  ((msub inc) (vmr 3)) ;=>>
  {:val 4}
  ;;mf operate on the mr value
  ((mf (fn [{:keys [vars val]}] (+ val (vars 'a)))) (vmr 3 {'a 2})))

(def find-first
  "returns the first matched element"
  (comp first filter))

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
  (map (mor [(mpred neg?) (mpred even?)]) [(vmr -1) (vmr 2) (vmr 3)]) ;=>>
  [{} {} failure?]
  ((mnor [0 (mpred even?) 1 (mpred odd?)] 'b) (vmr 3)) ;=>>
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
  (def mm (mmap {:a (mvar 'a (mval 4)) :b (mpred even?)}))
  (mm (vmr {:a 4 :b 0}))  ;=>>
  {:val {:a 4 :b 0} :vars {'a 4}}
  (mm (vmr {:a 4 :b 3})) ;=>>
  failure?)

;;## Sequence matchers
;;
;;The key function is `mseq` which contains sub-matchers, these sub-matchers
;;use clojure.zip's zipper as the `:val` of the mr, the convention is `zmr`,
;;The name convention for this kind of sub-matchers starting from `mz`, rather than `m`

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
                         (vapply (fn [z] (let [nv (:val result)] (-> (if (= node nv) z (zip/replace z nv)) zip/next))))
                         (merge-vars result)))))))))

^:rct/test
(comment
  ;;mzone matches a single value
  ((mzone (mvar 'a (mval 3))) (vmr (-> (zip/vector-zip [3 4]) zip/next))) ;=>>
  {:vars {'a 3}}
  ;;mzone fails when at the end
  ((mzone (mvar 'a (mval 3))) (vmr (-> (zip/vector-zip []) zip/next))) ;=>>
  failure?)

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
  (def bf (partial branching #(zero? (mod % 5)) #(apply str % %2)))
  (bf (range 1 8)) ;=> [1 2 3 4 "567"]
  (bf (range 1 3)) ;=> [1 2]
  (bf [1 5 6 8 10 11]) ;=>
  [1 "568" "1011"])

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

(defn mseq
  "a matcher matches a sequence by its `sub-matchers`"
  [sub-matchers]
  (let [children (expand-optionals sub-matchers)]
    (matcher :seq
             (fn [mr]
               (if-not (seqable? (:val mr))
                 (fail (str "expected seqable, got " (type (:val mr))) :seq (:val mr))
                 (let [result (reduce
                               (fn [zmr child]
                                 (let [child-result (child zmr)]
                                   (if (failure? child-result)
                                     (reduced child-result)
                                     child-result)))
                               (vmr (-> mr :val seq zip/seq-zip zip/next) (:vars mr)) children)]
                   (if (failure? result)
                     result
                     (vapply result zip/root))))))))

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
  ;;mseq fails when the value is not seqable
  ((mseq []) (vmr 3)) ;=>> failure?
  ;;mzterm matches the end of seq
  (map mterm [(vmr (-> (zip/vector-zip []) zip/next)) (vmr (-> (zip/vector-zip [3]) zip/next))]) ;=>>
  [identity failure?]
  ;;mseq matches one element and check ending
  (map (mseq [(mzone (mvar 'a (mpred even?))) mterm]) [(vmr [0]) (vmr [2 4]) (vmr [1])]) ;=>>
  [{:vars {'a 0}} failure? failure?]
  ;;mseq support nil element
  ((mseq [(mzone (mpred nil?))]) (vmr []))
  (map (mseq [(mzone (mpred nil?))]) [(vmr []) (vmr [nil])]) ;=>>
  [failure? {:val [nil]}]
  ;;mseq return value can be bound to a var
  ((mvar 'a (mseq [(mzone (mval 4))])) (vmr [4])) ;=>>
  {:vars {'a [4]}}
  ;;mseq support msub
  ((mseq [(mzone (mvar 'b (msub inc)))]) (vmr [3])) ;=>>
  {:val [4] :vars {'b 4}}
  ((mseq [(mzone (mval 3)) (mzone (mval 4)) mterm]) (vmr [3 4])) ;=>>
  {:val [3 4]}
  ;;moption captures an optional value
  (map (mseq [(mzoption (mvar 'a (mval 3))) (mzone (mval 0)) mterm]) [(vmr [3 0]) (vmr [0])]) ;=>>
  [{:vars {'a 3}} {}]
  ;;optional can be lazy
  (map (mseq [(mzoption (mvar 'a (mval 5)) true) (mzone (mvar 'b (mval 5))) mterm]) [(vmr [5]) (vmr [5 5])]) ;=>>
  [{:vars '{b 5}} {:vars '{a 5 b 5}}]
  ;;sub sequence value captured
  ((mseq [(mzsubseq (repeat 3 (mzone (mpred even?))) 'a) (mzone (mpred odd?)) mterm]) (vmr [2 4 6 7])) ;=>>
  {:vars {'a [2 4 6]}})

(defn mzrepeat
  "a sub-matcher repeats the `child` matcher with minimum length of `min-len`,
  Options:
   - `max-len` if it's length is flexable
   - `greedy?` apply when `max-len` is specified
   - `sym` if we bind the whole sub-seq to a symbol"
  ([child min-len {:keys [max-len greedy? sym]}]
   (let [zc (mzone child)]
     (if max-len
       (vary-meta zc assoc
                  ::optional (fn [_ nxt]
                               (->> (if greedy? (range max-len (dec min-len) -1) (range min-len (inc max-len)))
                                    (map (fn [len] (mzsubseq (cond->> nxt (pos? len) (concat (repeat len zc))) sym len)))
                                    (mor))))
       (mzsubseq (repeat min-len zc) sym)))))

^:rct/test
(comment
  ;;mzrepeat repeats child matchers for exact length
  (map (mseq [(mzrepeat (mpred even?) 3 {:sym 'a}) mterm]) [(vmr [8 10 12]) (vmr [8 10]) (vmr [8 10 12 4])]) ;=>>
  [{:vars {'a [8 10 12]}} failure? failure?]
  ;;mzrepeat can have optionally length
  (map (mseq [(mzrepeat (mpred even?) 0 {:max-len 2 :sym 'a}) (mzone (mval 3)) mterm])
       [(vmr [3]) (vmr [0 3]) (vmr [0 2 3]) (vmr [0 2 4 3])]) ;=>>
  [{:vars '{a []}} {:vars '{a [0]}} {:vars '{a [0 2]}} failure?]
  ((mseq [(mzoption (mvar 'len (mpred (complement neg-int?)))) (mzoption (mvar 'sym (mpred symbol?)))]) (vmr [3 'a])))

;;## Pattern
;;
;; Pattern is a mini language using common clojure data, it can translate to matcher functions.

(defn mchain [children]
  (matcher :chain
           (fn [mr]
             (reduce (fn [mr' child]
                       (let [result (child mr')]
                         (if (failure? result)
                           (reduced result)
                           result)))
                     mr children))))

(def ^:private matcher-arg-specs
  "Specification of expected arguments for each (? :type ...) matcher"
  {:pred {:args "(? :pred <fn>)"
          :matcher (mzone (mvar 'pred (mpred fn?)))}
   :val  {:args "(? :val <value>)"
          :matcher (mzone (mvar 'val identity))}
   :map  {:args "(? :map <map>)"
          :matcher (mzone (mvar 'map (mpred map?)))}
   :1    {:args "(? :1 <matcher>)"
          :matcher (mzone (mvar 'child (mpred matcher-type)))}
   :?    {:args "(? :? <matcher>)"
          :matcher (mzone (mvar 'child (mpred matcher-type)))}
   :-    {:args "(? :- <matcher> <len> [<max-len>] [<sym>])"
          :matcher (mzsubseq [(mzone (mvar 'child (mpred matcher-type)))
                              (mzone (mvar 'len (mpred (complement neg-int?))))
                              (mzoption (mvar 'max-len (mpred (complement neg-int?))))
                              (mzoption (mvar 'sym (mpred symbol?)))]
                             nil)}
   :seq  {:args "(? :seq [<matchers>...])"
          :matcher (mzone (mvar 'children (mpred #(every? matcher-type %))))}
   :var  {:args "(? :var <sym> <matcher>)"
          :matcher (mzsubseq [(mzone (mvar 'sym (mpred symbol?)))
                              (mzone (mvar 'child (mpred matcher-type)))]
                             nil)}})

(defn matcher-args [t]
  (if-let [spec (get matcher-arg-specs t)]
    (:matcher spec)
    (throw (ex-info (str "Unknown matcher type: " t
                         ". Valid types: " (keys matcher-arg-specs))
                    {:type t :valid-types (keys matcher-arg-specs)}))))

(defn make-matcher [{:keys [vars]}]
  (case (get vars 'type)
    :pred (mpred (get vars 'pred))
    :val (mval (get vars 'val))
    :map (mmap (get vars 'map))
    :1 (mzone (get vars 'child))
    :? (mzoption (get vars 'child))
    :- (mzrepeat (get vars 'child) (get vars 'len) {:max-len (get vars 'max-len) :sym (get vars 'sym)})
    :seq (mseq (get vars 'children))
    :var (mvar (get vars 'sym) (get vars 'child))))

(defn- check-matcher-args
  "Check matcher args and throw descriptive error on failure"
  [zmr]
  (let [matcher-type (get-in zmr [:vars 'type])
        arg-matcher (matcher-args matcher-type)
        result (arg-matcher zmr)]
    (if (failure? result)
      (let [spec (get matcher-arg-specs matcher-type)]
        (throw (ex-info (str "Invalid arguments for (? " matcher-type " ...)\n"
                             "  Expected: " (:args spec) "\n"
                             "  Error: " (:reason result))
                        {:matcher-type matcher-type
                         :expected (:args spec)
                         :failure result})))
      result)))

(def core-rule
  (mchain [(mseq [(mzone (mval '?))
                  (mzone (mvar 'type (mpred keyword?)))
                  check-matcher-args])
           (mf make-matcher)]))

(defn rule-of [rule ptn] (some-> ptn vmr rule :val))

^:rct/test
(comment
  ((rule-of core-rule (list '? :pred odd?)) (vmr 3)) ;=>>
  {:val 3}
  ((rule-of core-rule '(? :val 5)) (vmr 5)) ;=>>
  {:val 5})

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
  "Parse a list-form named variable like (?x even? [2 4] !).
   Returns nil if not a valid list-var, or a map with:
   - :sym - the variable name (symbol or nil for wildcard)
   - :pred - predicate function (or nil)
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
                 result {:sym sym :pred nil :min-len nil :max-len nil :greedy? false}]
            (if (empty? args)
              result
              (let [[arg & rest-args] args]
                (cond
                  ;; Greedy flag
                  (= '! arg)
                  (recur rest-args (assoc result :greedy? true))

                  ;; Length vector [min max]
                  (and (vector? arg) (= 2 (count arg))
                       (every? #(and (number? %) (not (neg? %))) arg))
                  (let [[min-len max-len] arg]
                    (recur rest-args (assoc result :min-len min-len :max-len max-len)))

                  ;; Predicate function (actual fn or symbol to resolve)
                  (fn? arg)
                  (recur rest-args (assoc result :pred arg))

                  ;; Predicate symbol - resolve it
                  (symbol? arg)
                  (if-let [resolved (resolve arg)]
                    (recur rest-args (assoc result :pred @resolved))
                    nil)  ;; Symbol doesn't resolve - invalid

                  ;; Invalid - not a list-var
                  :else nil)))))))))

(defn- scalar-element [x]
  (cond
    (matcher-type x) x
    (fn? x) (mpred x)
    :else (mval x)))

(defn ptn->matcher
  ([ptn]
   (ptn->matcher ptn [core-rule]))
  ([ptn rules]
   (when-let [mch (mor rules)]
     ;; Use prewalk to handle list-var patterns before children are transformed,
     ;; then postwalk for everything else
     (let [prewalk-fn (fn [x]
                        ;; Check list-var pattern first (before children transform)
                        (if-let [parsed (parse-list-var x)]
                          (let [{:keys [sym pred min-len max-len greedy?]} parsed
                                child (if pred (mpred pred) wildcard)]
                            (if min-len
                              (let [effective-max (if (zero? max-len) 100 max-len)]
                                (mzrepeat child min-len {:max-len effective-max :greedy? greedy? :sym sym}))
                              (if sym (mvar sym child) child)))
                          x))
           postwalk-fn (fn [x] (or (rule-of mch x) x))]
       (->> ptn
            (walk/prewalk prewalk-fn)
            (walk/postwalk postwalk-fn))))))

^:rct/test
(comment
  ((ptn->matcher '(? :map {:a (? :var a (? :val 3)) :b (? :map {:c (? :var a (? :val 3))})})) (vmr {:a 3 :b {:c 3}})) ;=>>
  {:vars '{a 3}}
  ((ptn->matcher '(? :seq [(? :? (? :val 1)) (? :1 (? :val 10))])) (vmr [10])) ;=>>
  {:val [10]}
  ((ptn->matcher (list '? :seq [(list '? :- (list '? :pred odd?) 1 2 'a)])) (vmr [1 3 5])) ;=>>
  {:val [1]})

(defn- wrap-element [x]
  (let [mt (matcher-type x)] (cond-> x (not mt) scalar-element (not= mt :subseq) mzone)))

(defn- seq-matcher [x]
  (-> (map wrap-element x) (concat [mterm]) mseq))

(def vector-rule
  (mchain [(mpred #(and (vector? %) (not (map-entry? %))))
           (msub seq-matcher)]))

(defn- map-matcher [x]
  (-> (map (fn [[k v]] [k (scalar-element v)]) x) mmap))

(def map-rule
  (mchain [(mpred map?)
           (msub map-matcher)]))

^:rct/test
(comment
  ((ptn->matcher '{:a (? :var a (? :val 5))} [map-rule core-rule]) (vmr {:a 5})) ;=>>
  {:val {:a 5}})

(defn named-var?
  "Parse a symbol as a named pattern variable.
   Returns `[base-name flags]` when `v` matches pattern variable syntax.
   Supported forms:
   - ?x      basic binding
   - ?x?     optional (0 or 1)
   - ?x+     one or more (lazy)
   - ?x*     zero or more (lazy)
   - ?x+!    one or more (greedy)
   - ?x*!    zero or more (greedy)
   - ??x     sequence binding"
  [v]
  (when (symbol? v)
    (when-let [[_ prefix n suffix greedy] (re-matches #"(\?[\?]?)([\p{Alnum}-_]+)([\?\+\*]?)(\!?)" (name v))]
      [(when-not (= "_" n) (symbol n))
       (cond-> #{}
         (= suffix "?") (conj :optional)
         (= suffix "+") (conj :one-or-more)
         (= suffix "*") (conj :zero-or-more)
         (= greedy "!") (conj :greedy)
         (= prefix "??") (conj :seq))])))

^:rct/test
(comment
  (named-var? '?x3) ;=>>
  ['x3 #{}]
  (named-var? '?x?) ;=>>
  ['x #{:optional}]
  (named-var? '?x+) ;=>>
  ['x #{:one-or-more}]
  (named-var? '?x*) ;=>>
  ['x #{:zero-or-more}]
  (named-var? '?x+!) ;=>>
  ['x #{:one-or-more :greedy}]
  (named-var? '?x*!) ;=>>
  ['x #{:zero-or-more :greedy}]
  (named-var? '?_) ;=>>
  [nil #{}])

(defn- repeat-matcher
  "Create a repeating matcher for + and * quantifiers.
   Default is lazy (match minimum), use greedy? true to match maximum."
  [sym min-len greedy?]
  ;; Use mzrepeat with a large max-len for practical unbounded matching
  (mzrepeat wildcard min-len {:max-len 100 :greedy? greedy? :sym sym}))

(def named-var-rule
  (mchain [(mchain [(msub named-var?)
                    (mseq [(mzone (mvar 'sym identity)) (mzone (mvar 'flags identity))])])
           (mf (fn [{{:syms [sym flags]} :vars}]
                 (cond
                   (flags :one-or-more)  (repeat-matcher sym 1 (flags :greedy))
                   (flags :zero-or-more) (repeat-matcher sym 0 (flags :greedy))
                   :else (cond->> wildcard
                           sym (mvar sym)
                           (flags :optional) mzoption
                           (flags :seq) seq-matcher))))]))

(def core-rules [named-var-rule vector-rule map-rule core-rule])

(defn query
  "Match pattern against data and return variable bindings, or nil if no match"
  [ptn data]
  (let [result ((ptn->matcher ptn core-rules) (vmr data))]
    (when-not (failure? result)
      (:vars result))))

(defn query-with-failure
  "Match pattern against data and return either {:vars ...} on success or the MatchFailure on failure"
  [ptn data]
  (let [result ((ptn->matcher ptn core-rules) (vmr data))]
    (if (failure? result)
      result
      {:vars (:vars result)})))

^:rct/test
(comment
  ((rule-of named-var-rule '?x) (vmr 5)) ;=>>
  {:vars '{x 5}}
  (query '{:a ?a :b {:c ?a}} {:a 3 :b {:c 3}}) ;=>>
  '{a 3}
  (query '[3 ?_ ?a?] [3 0 4]) ;=>>
  '{a 4}
  ;;?x+ matches one or more elements
  (query '[1 ?rest+] [1 2 3 4]) ;=>>
  '{rest [2 3 4]}
  (query '[?first ?rest+] [1 2 3]) ;=>>
  '{first 1 rest [2 3]}
  (query '[?rest+] []) ;=>
  nil
  ;;?x* matches zero or more elements
  (query '[1 ?rest*] [1 2 3]) ;=>>
  '{rest [2 3]}
  (query '[1 ?rest*] [1]) ;=>>
  '{rest []}
  ;;lazy vs greedy: lazy matches minimum, greedy matches maximum
  ;; For [?a* ?b*], both can vary so lazy/greedy matters
  (query '[?a* ?b*] [1 2 3]) ;=>>
  '{a () b (1 2 3)}                     ; lazy: ?a* takes minimum (0)
  (query '[?a*! ?b*] [1 2 3]) ;=>>
  '{a (1 2 3) b ()}                     ; greedy: ?a*! takes maximum
  ;; For [?a+ ?b+], both must match at least 1
  (query '[?a+ ?b+] [1 2 3]) ;=>>
  '{a (1) b (2 3)}                      ; lazy: ?a+ takes minimum (1)
  (query '[?a+! ?b+] [1 2 3]) ;=>>
  '{a (1 2) b (3)}                      ; greedy: ?a+! takes maximum
  ;;query returns nil on failure (backward compatible)
  (query '{:a 5} {:a 3}) ;=>
  nil
  ;;query-with-failure returns detailed failure info
  (query-with-failure '{:a 5} {:a 3}) ;=>>
  {:reason #"value mismatch" :path [:a] :depth 1}
  ;;nested failure with path trace
  (query-with-failure '{:a {:b 10}} {:a {:b 20}}) ;=>>
  {:path [:a :b] :depth 2}
  ;;list-form named variables: (?x pred? [min max]? !?)
  (query '[(?x even?)] [4]) ;=>>
  '{x 4}
  (query '[(?x even?)] [3]) ;=>
  nil
  ;;with length range [min max]
  (query '[(?x [2 4])] [1 2 3]) ;=>>
  '{x (1 2 3)}
  (query '[(?x [2 4])] [1]) ;=>
  nil
  ;;with predicate and length
  (query '[(?x even? [2 3])] [2 4 6]) ;=>>
  '{x (2 4 6)}
  (query '[(?x even? [2 3])] [2 4 5]) ;=>
  nil
  ;;unbounded (max=0 means match to end)
  (query '[(?x [1 0])] [1 2 3]) ;=>>
  '{x (1 2 3)}
  ;;lazy vs greedy with list-var
  (query '[(?x [0 5]) ?y*] [1 2 3]) ;=>>
  '{x () y (1 2 3)}
  (query '[(?x [0 5] !) ?y*] [1 2 3]) ;=>>
  '{x (1 2 3) y ()})

;;## Utils

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

^:rct/test
(comment
  ((substitute '(+ 5 ?x ?y)) {:vars {'x 3 'y 0}}) ;=>>
  8
  ((substitute '(* ?a (+ ?b 1))) {:vars {'a 2 'b 3}}) ;=>>
  8
  ((substitute '{:result ?val}) {:vars {'val 42}}) ;=>>
  {:result 42})
