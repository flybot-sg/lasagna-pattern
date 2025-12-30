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

(defrecord ValMatchResult [val vars]
  IMatchResult
  (-fapply [this f]
    (let [v (f this)]
      (when-not (= v ::none)
        (ValMatchResult. v vars))))
  (-bind [this sym v]
    (let [old-v (get vars sym ::not-found)]
      (condp = old-v
        ::not-found (ValMatchResult. val (assoc vars sym v))
        v this
        nil))))

(defn vapply
  "apply `f` to a `mr`'s `:val`"
  [mr f]
  (-fapply mr (comp f :val)))

(defn fit
  "returns the `mr` if `pred` to its `:val` is not false"
  [mr pred]
  (vapply mr (fn [val] (if (pred val) val ::none))))

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
  "a pred matcher matchers when mr's value equals v"
  [v]
  (mpred #(= v %)))

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
  (with-meta (fn [mr] (when-let [mr' (child mr)] (-bind mr' sym (:val mr'))))
    (meta child)))

^:rct/test
(comment
  ;;mpred succeed when `pred` success
  (map (mvar 'a (mpred even?)) [(vmr 4) (vmr 3)]) ;=>>
  [{:val 4 :vars {'a 4}} nil]
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
  "a matcher tests `matchers` and returns the first non-nil result"
  [matchers]
  (matcher :or
           (fn [mr]
             (->> matchers (map #(% mr)) (find-first identity)))))

(defn mnor
  "a matcher tests sequentially `kv-matchers`, which is a key to a matcher,
  bind the successful key to `sym`"
  [kv-matchers sym]
  (matcher :nor
           (fn [mr]
             (let [[k mr']
                   (->> (partition 2 kv-matchers)
                        (map (fn [[k mch]] [k (mch mr)]))
                        (find-first second))]
               (when mr'
                 (if sym (-bind mr sym k) mr'))))))

^:rct/test
(comment
  (map (mor [(mpred neg?) (mpred even?)]) [(vmr -1) (vmr 2) (vmr 3)]) ;=>>
  [{} {} nil]
  ((mnor [0 (mpred even?) 1 (mpred odd?)] 'b) (vmr 3)) ;=>>
  {:vars {'b 1}})

;;## Map matcher

(defn mmap
  "a matcher matches on `matcher-map`, which is a key to matcher map"
  [matcher-map]
  (matcher :map
           (fn [mr]
             (let [m (:val mr)]
               (when (map? m)
                 (reduce
                  (fn [mr' [k mch]]
                    (if-let [vmr (some-> (vmr (get m k)) (mch))]
                      (-> mr' (update :val conj [k (:val vmr)]) (merge-vars vmr))
                      (reduced nil)))
                  mr matcher-map))))))

^:rct/test
(comment
  (def mm (mmap {:a (mvar 'a (mval 4)) :b (mpred even?)}))
  (mm (vmr {:a 4 :b 0}))  ;=>>
  {:val {:a 4 :b 0} :vars {'a 4}}
  (mm (vmr {:a 4 :b 3})) ;=>
  nil)

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
               (when-not (zip/end? zip)
                 (let [node (zip/node zip)]
                   (when-let [vmr (child (assoc zmr :val node))]
                     (-> zmr
                         (vapply (fn [z] (let [nv (:val vmr)] (-> (if (= node nv) z (zip/replace z nv)) zip/next))))
                         (merge-vars vmr)))))))))

^:rct/test
(comment
  ;;mzone matches a single value
  ((mzone (mvar 'a (mval 3))) (vmr (-> (zip/vector-zip [3 4]) zip/next))) ;=>>
  {:vars {'a 3}}
  ;;mzone fails when at the end
  ((mzone (mvar 'a (mval 3))) (vmr (-> (zip/vector-zip []) zip/next))) ;=>
  nil)

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
   (matcher :subseq
            (fn [zmr]
              (when-let [[rslt vals] (reduce (fn [[zmr' acc] child]
                                               (let [zv (:val zmr')]
                                                 (if-let [zmr'' (child zmr')]
                                                   [zmr'' (cond-> acc
                                                            (not (zip/end? zv)) (conj (zip/node zv)))]
                                                   (reduced nil))))
                                             [zmr []] sub-matchers)]
                (cond-> rslt sym (-bind sym (cond->> vals len (take len)))))))))

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

(defn mseq
  "a matcher matches a sequence by its `sub-matchers`"
  [sub-matchers]
  (let [children (branching optional (fn [c nxt] ((optional c) c nxt)) sub-matchers)]
    (matcher :seq
             (fn [mr]
               (when (seqable? (:val mr))
                 (some-> (reduce
                          (fn [zmr child] (or (child zmr) (reduced nil)))
                          (vmr (-> mr :val seq zip/seq-zip zip/next) (:vars mr)) children)
                         (vapply zip/root)))))))

(def mterm
  "a matcher tests if it is the end of a seq"
  (matcher :term (mpred zip/end?)))

^:rct/test
(comment
  ;;mseq fails when the value is not seqable
  ((mseq []) (vmr 3)) ;=> nil
  ;;mzterm matches the end of seq
  (map mterm [(vmr (-> (zip/vector-zip []) zip/next)) (vmr (-> (zip/vector-zip [3]) zip/next))]) ;=>>
  [identity nil]
  ;;mseq matches one element and check ending
  (map (mseq [(mzone (mvar 'a (mpred even?))) mterm]) [(vmr [0]) (vmr [2 4]) (vmr [1])]) ;=>>
  [{:vars {'a 0}} nil nil]
  ;;mseq support nil element
  ((mseq [(mzone (mpred nil?))]) (vmr []))
  (map (mseq [(mzone (mpred nil?))]) [(vmr []) (vmr [nil])]) ;=>>
  [nil {:val [nil]}]
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
  ([child min-len & {:keys [max-len greedy? sym]}]
   (let [zc (mzone child)]
     (if max-len
       (with-meta zc
         {::optional (fn [_ nxt]
                       (->> (if greedy? (range max-len (dec min-len) -1) (range min-len (inc max-len)))
                            (map (fn [len] (mzsubseq (cond->> nxt (pos? len) (concat (repeat len zc))) sym len)))
                            (mor)))})
       (mzsubseq (repeat min-len zc) sym)))))

^:rct/test
(comment
  ;;mzrepeat repeats child matchers for exact length
  (map (mseq [(mzrepeat (mpred even?) 3 :sym 'a) mterm]) [(vmr [8 10 12]) (vmr [8 10]) (vmr [8 10 12 4])]) ;=>>
  [{:vars {'a [8 10 12]}} nil nil]
  ;;mzrepeat can have optionally length
  (map (mseq [(mzrepeat (mpred even?) 0 :max-len 2 :sym 'a) (mzone (mval 3)) mterm])
       [(vmr [3]) (vmr [0 3]) (vmr [0 2 3]) (vmr [0 2 4 3])]) ;=>>
  [{:vars '{a []}} {:vars '{a [0]}} {:vars '{a [0 2]}} nil])

;;## Pattern
;;
;; Pattern is a mini language using common clojure data, it can translate to matcher functions.

(defn mchain [children]
  (matcher :chain
           (fn [mr]
             (reduce (fn [mr' child] (or (child mr') (reduced nil))) mr children))))

(defn matcher-args [t]
  (case t
    :pred (mzone (mvar 'pred (mpred fn?)))
    :val (mzone (mvar 'val identity))
    :map (mzone (mvar 'map (mpred map?)))
    :1 (mzone (mvar 'child (mpred matcher-type)))
    :? (mzone (mvar 'child (mpred matcher-type)))
    :seq (mzone (mvar 'children (mpred #(every? matcher-type %))))
    :var (mzsubseq [(mzone (mvar 'sym (mpred symbol?)))
                    (mzone (mvar 'child (mpred matcher-type)))] nil)))

(defn make-matcher [{:keys [vars]}]
  (case (get vars 'type)
    :pred (mpred (get vars 'pred))
    :val (mval (get vars 'val))
    :map (mmap (get vars 'map))
    :1 (mzone (get vars 'child))
    :? (mzoption (get vars 'child))
    :seq (mseq (get vars 'children))
    :var (mvar (get vars 'sym) (get vars 'child))))

(def core-rule
 (mchain [(mseq [(mzone (mval '?))
                 (mzone (mvar 'type (mpred keyword?)))
                 (fn [zmr] ((matcher-args (get-in zmr [:vars 'type])) zmr))])
          (mf make-matcher)]))

(defn run-rule [rule ptn value] ((-> ptn vmr rule :val) (vmr value)))

^:rct/test
(comment
  (run-rule core-rule (list '? :pred odd?) 3) ;=>>
  {:val 3}
  (run-rule core-rule '(? :val 5) 5) ;=>>
  {:val 5}) 

(defn- scalar-element [x]
  (cond
    (matcher-type x) x
    (fn? x) (mpred x)
    :else (mval x)))

(defn ptn->matcher
  ([ptn]
   (ptn->matcher ptn [core-rule]))
  ([ptn rules]
   (when-let [mch (mchain rules)]
    (some->> (walk/postwalk (fn [x] (or (some-> (vmr x) mch :val) x)) (vmr ptn))
             :val))))

^:rct/test
(comment
  ((ptn->matcher '(? :map {:a (? :var a (? :val 3)) :b (? :map {:c (? :var a (? :val 3))})})) (vmr {:a 3 :b {:c 3}})) ;=>>
  {:vars '{a 3}}
  ((ptn->matcher '(? :seq [(? :? (? :val 1)) (? :1 (? :val 10))])) (vmr [10])) ;=>>
  {:val [10]})

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
  (run-rule vector-rule [identity] [3]) ;=>>
  {:val [3]}
  (run-rule map-rule {:a 5} {:a 5}) ;=>>
  {:val {:a 5}})

(defn named-var?
  "Parse a symbol as a named pattern variable.
   Returns `[base-name seq?]` when `v` has the `?x`, `??x` or
   forms, otherwise `nil`."
  [v]
  (when (symbol? v)
    (when-let [[_ s n optional?] (re-matches #"(\?[\?]?)([\p{Alnum}-_]+)(\??)" (name v))]
      [(when-not (= "_" n) (symbol n)) (cond-> #{} (= optional? "?") (conj :optional) (= s "??") (conj :seq))])))

:rct/test
(comment
  (named-var? '?x3) ;=>>
  ['x3 #{}]
  (named-var? '?x?) ;=>>
  ['x #{:optional}]
  (named-var? '?_))

(def named-var-rule
  (mchain [(mchain [(msub named-var?) 
                    (mseq [(mzone (mvar 'sym identity)) (mzone (mvar 'flags identity))])])
           (mf (fn [{{:syms [sym flags]} :vars}]
                 (cond-> (mvar sym identity)
                   (flags :optional) mzoption
                   (flags :seq) seq-matcher)))]))

^:rct/test
(comment
  (run-rule named-var-rule '?x 5) ;=>>
  {:vars '{x 5}})


