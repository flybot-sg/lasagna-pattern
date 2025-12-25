(ns sg.flybot.pullable.core
  (:require
   [clojure.zip :as zip]))

;;# Matcher and matcher result
;;
;; A matcher is a function takes a match result (mr) and returns a mr, or nil if not matching.
;;
;; A matcher result is a map contains `:val` as it value, `:vars` is a map contains
;; matching variable values.

(defprotocol IMatchResult
  (-apply [mr f]
    "apply `f` to `mr`, returns a mr of the result")
  (-bind [mr sym v]
    "bind symbol `sym` with `v` in the result"))

(defrecord ValMatchResult [val vars]
  IMatchResult
  (-apply [_ f]
    (let [v (f val)]
      (when-not (= v ::none)
        (ValMatchResult. (f val) vars))))
  (-bind [this sym v]
    (let [old-v (get vars sym ::not-found)]
      (condp = old-v
        ::not-found (ValMatchResult. val (assoc vars sym v))
        v this
        nil))))

(defn fit [mr pred]
  (-apply mr (fn [val] (if (pred val) val ::none))))

(defn vmr
  ([val]
   (vmr val nil))
  ([val vars]
   (ValMatchResult. val vars)))

^:rct/test
(comment
  (-apply (vmr 3) inc) ;=>>
  {:val 4}
  (-bind (vmr 3) 'a 4) ;=>> {:vars {'a 4}}
  (fit (vmr 3) odd?) ;=>>
  {:val 3})

;;## Implementation of matchers 

(defn mpred
  [pred]
  #(fit % pred))

(defn mval
  "a pred matcher matchers when mr's value equals v"
  [v]
  (mpred #(= v %)))

(defn msub
  [f]
  #(-apply % f))

(defn mvar [sym child]
  (fn [mr] (when-let [mr' (child mr)] (-bind mr' sym (:val mr')))))

^:rct/test
(comment
  ;;mpred succeed when `pred` success
  (map (mvar 'a (mpred even?)) [(vmr 4) (vmr 3)]) ;=>>
  [{:val 4 :vars {'a 4}} nil]
  ((mval 3) (vmr 3)) ;=>>
  {:val 3}
  ;;msub operates on `val` of a mr
  ((msub inc) (vmr 3)) ;=>>
  {:val 4})

(defn merge-vars
  [mr other]
  (if-let [vo (:vars other)]
    (vmr (:val mr) (merge (:vars mr) vo))
    mr))

;;## Map matcher
(defn mmap
  [k-matchers]
  (fn [mr]
    (let [m (:val mr)]
      (when (map? m)
        (reduce
         (fn [mr' [k mch]]
           (if-let [vmr (some-> (vmr (get m k)) (mch))]
             (-> mr' (update :val conj [k (:val vmr)]) (merge-vars vmr))
             (reduced nil)))
         mr k-matchers)))))

^:rct/test
(comment
  (def mm (mmap [[:a (mvar 'a (mval 4))] [:b (mpred even?)]]))
  (mm (vmr {:a 4 :b 0}))  ;=>>
  {:val {:a 4 :b 0} :vars {'a 4}}
  (mm (vmr {:a 4 :b 3})) ;=>
  nil)

;;## Sequence matchers

(defn mzone
  [child]
  (fn [zmr]
    (let [zip (:val zmr)]
      (when-not (zip/end? zip)
        (let [node (zip/node zip)]
          (when-let [vmr (child (assoc zmr :val node))]
            (-> zmr
                (-apply (fn [z] (let [nv (:val vmr)] (-> (if (= node nv) z (zip/replace z nv)) zip/next))))
                (merge-vars vmr))))))))

^:rct/test
(comment
  ;;mzone matches a single value
  ((mzone (mvar 'a (mval 3))) (vmr (-> (zip/vector-zip [3 4]) zip/next))) ;=>>
  {:vars {'a 3}}
  ;;mzone fails when at the end
  ((mzone (mvar 'a (mval 3))) (vmr (-> (zip/vector-zip []) zip/next))) ;=>
  nil)

(def find-first (comp first filter))

(defn mor
  [matchers]
  (fn [mr]
    (->> matchers (map #(% mr)) (find-first identity))))

(defn mnor
  [kv-matchers sym]
  (fn [mr]
    (let [[k mr']
          (->> (partition 2 kv-matchers)
               (map (fn [[k mch]] [k (mch mr)]))
               (find-first second))]
      (when mr'
        (if sym (-bind mr sym k) mr')))))

^:rct/test
(comment
  (map (mor [(mpred neg?) (mpred even?)]) [(vmr -1) (vmr 2) (vmr 3)]) ;=>>
  [{} {} nil]
  ((mnor [0 (mpred even?) 1 (mpred odd?)] 'b) (vmr 3)) ;=>>
  {:vars {'b 1}})

(defn branching
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
  ([children sym]
   (mzsubseq children sym nil))
  ([children sym len]
   (fn [zmr]
     (when-let [[rslt vals] (reduce (fn [[zmr' acc] child]
                                      (let [zv (:val zmr')]
                                        (if-let [zmr'' (child zmr')]
                                          [zmr'' (cond-> acc
                                                   (not (zip/end? zv)) (conj (zip/node zv)))]
                                          (reduced nil))))
                                    [zmr []] children)]
       (cond-> rslt sym (-bind sym (cond->> vals len (take len))))))))

(defn moption
  "Optional subsequence matcher"
  ([child]
   (moption child false))
  ([child lazy?]
   (with-meta child {::optional (fn [c nxt]
                                  (let [m1 (mzsubseq (cons (mzone c) nxt) nil)
                                        m0 (mzsubseq nxt nil)]
                                    (mor (if lazy? [m0 m1] [m1 m0]))))})))

(defn mseq
  [children]
  (let [children (branching optional (fn [c nxt] ((optional c) c nxt)) children)]
    (fn [mr]
      (some-> (reduce
               (fn [zmr child] (or (child zmr) (reduced nil)))
               (vmr (-> mr :val seq zip/seq-zip zip/next) (:vars mr)) children)
              (-apply zip/root)))))

(def mzterm (mpred zip/end?))

^:rct/test
(comment
  ;;mzterm matches the end of seq
  (map mzterm [(vmr (-> (zip/vector-zip []) zip/next)) (vmr (-> (zip/vector-zip [3]) zip/next))]) ;=>>
  [identity nil]
  ;;mseq matches one element and check ending
  (map (mseq [(mzone (mvar 'a (mpred even?))) mzterm]) [(vmr [0]) (vmr [2 4]) (vmr [1])]) ;=>>
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
  ((mseq [(mzone (mval 3)) (mzone (mval 4)) mzterm]) (vmr [3 4])) ;=>>
  {:val [3 4]}
  ;;moption captures an optional value
  (map (mseq [(moption (mvar 'a (mval 3))) (mzone (mval 0)) mzterm]) [(vmr [3 0]) (vmr [0])]) ;=>>
  [{:vars {'a 3}} {}]
  ;;optional can be lazy
  (map (mseq [(moption (mvar 'a (mval 5)) true) (mzone (mvar 'b (mval 5))) mzterm]) [(vmr [5]) (vmr [5 5])]) ;=>>
  [{:vars '{b 5}} {:vars '{a 5 b 5}}]
  ;;sub sequence value captured
  ((mseq [(mzsubseq (repeat 3 (mzone (mpred even?))) 'a) (mzone (mpred odd?)) mzterm]) (vmr [2 4 6 7])) ;=>>
  {:vars {'a [2 4 6]}})

(defn mzrepeat
  ([child min-len & {:keys [max-len greedy? sym]}]
   (let [zc (mzone child)]
     (if max-len
       (let [var-len (- max-len min-len)]
         (with-meta zc
           {::optional (fn [c nxt]
                         (->> (if greedy? (range max-len (dec min-len) -1) (range min-len (inc max-len)))
                              (map (fn [len] (mzsubseq (cond->> nxt (pos? len) (concat (repeat len zc))) sym len)))
                              (mor)))}))
       (mzsubseq (repeat min-len zc) sym)))))

^:rct/test
(comment
  ;;mzrepeat repeats child matchers for exact length
  (map (mseq [(mzrepeat (mpred even?) 3 :sym 'a) mzterm]) [(vmr [8 10 12]) (vmr [8 10]) (vmr [8 10 12 4])]) ;=>>
  [{:vars {'a [8 10 12]}} nil nil]
  ;;mzrepeat can have optionally length
  ((mseq [(mzrepeat (mpred even?) 1 :max-len 2 :sym 'a) mzterm]) (vmr [8 10]))
  (map (mseq [(mzrepeat (mpred even?) 0 :max-len 2 :sym 'a) (mzone (mval 3)) mzterm])
       [(vmr [3]) (vmr [0 3]) (vmr [0 2 3]) (vmr [0 2 4 3])]))

;; (defn mlazy-subseq
;;   ([child min-len max-len greedy?]
;;    (mlazy-subseq child min-len max-len greedy? nil))
;;   ([child min-len max-len greedy? var-sym]
;;    (matcher
;;     child
;;     :subseq ::optional (fn [c nxt]
;;                          (let [mth (->> (if greedy? (range max-len (dec min-len) -1) (range min-len (inc max-len)))
;;                                         (map (fn [len] (if (zero? len) (apply msubseq nxt) (apply msubseq (mrest c len) nxt))))
;;                                         (mor))]
;;                            (cond->> mth var-sym (mvar var-sym)))))))

;; (defn mseq
;;   "Top level sequence matcher.
;;    Treats the current `:val` as the full sequence, stores it in `:rest`,
;;    runs `matchers` left-to-right and succeeds only when the whole
;;    sequence is consumed."
;;   [matchers]
;;   (let [matchers (branching optional (fn [c nxt] ((optional c) c nxt)) matchers)]
;;     (matcher
;;      (fn [mr]
;;        (let [v (:val mr)]
;;          (when (seqable? v)
;;            (-> (reduce (fn [mr' mch]
;;                          (or (mch mr') (reduced nil)))
;;                        (assoc mr :rest v)
;;                        matchers)
;;                (dissoc :rest)))))
;;      :seq)))

;; ^:rct/test
;; (comment
;;   ;;mone matches a single sequence value
;;   (def mo (mvar 'a (mone (mval 1))))
;;   (mo {:val [1] :rest [1]})             ;=>> {:vars {'a [1]} :rest []}
;;   (mo {:val [2] :rest [2]})             ;=> nil
;;   (mo {:val [] :rest []})               ;=> nil

;;   ;;mseq matches a whole sequence
;;   (def sm (mseq [(mone (mvar 'a (mval 1))) (mone (mpred zero?)) (mone (mpred odd?)) mnone]))
;;   (sm {:val [1 0 3]})                   ;=>> {:vars {'a 1}}
;;   (sm {:val [1 0 3 2]})                 ;=> nil

;;   ;;complex patterns
;;   (def sm (mseq [(mone (mvar 'b (mval 1))) (mone (mpred zero?)) (mvar 'a (mrest (mpred odd?))) mnone]))
;;   (sm {:val [1 0 1 3 5]})               ;=>> {:vars {'a [1 3 5]}}
;;   (sm {:val [1 0 1 3 5 2]})             ;=> nil

;;   ;;moption support
;;   (map (mseq [(moption (mvar 'opt (mval 2))) (mone (mval 1))]) [{:val [1]} {:val [2 1]}]) ;=>>
;;   [{} {:vars {'opt 2}}]
;;   ((mseq [(mone (mval 4)) (moption (mval 2))]) {:val [4]}) ;=>> {:val [4]}

;;   ;;mrest support len
;;   (map (mseq [(mrest (mpred even?) 2) mnone]) [{:val [0]} {:val [0 2]} {:val [0 2 4]}]) ;=>>
;;   [nil {} nil]

;;   ;;TODO wrong result
;;   ((mseq [(mvar 'a (msubseq (mone wildcard) (mone wildcard))) (mone (mval 10)) mnone]) {:val [5 6 10]}) ;=>>
;;   {:vars {'a [5 6]}}
;;   ((mseq [(mor [(mvar 'a (msubseq (mone (mval 3)))) (msubseq (mone (mval 4)) (mone (mval 5)))])]) {:val [4 5]})
;;   ;;more branching
;;   (def mth (mseq [(mlazy-subseq (mval 10) 0 2 false 'a)  mnone]))
;;   (map mth [{:val []} {:val [10 10]} {:val [10 10 10]}]) ;=>>
;;   [{:val []} {:val [10 10]} nil]
;;   ;;greedy
;;   (map (mseq [(mlazy-subseq (mval 10) 0 2 true 'a) (mone (mval 20)) mnone])
;;        [{:val [10 20]} {:val [10 10 20]} {:val [10 10 10 20]}]) ;=>>
;;   [{:vars {'a [10]}} {:vars {'a [10 10]}} nil])

;; ;;## Misc matchers

;; (defn mf [f child]
;;   (matcher (fn [mr] (when-let [mr (child mr)] (f mr))) :sub))

;; (defn msub [f child]
;;   (mf #(update % :val f) child))

;; (defn mmulti
;;   "Higher order matcher that looks up variable `sym` in `:vars`, calls
;;    `(f v)` to obtain a matcher and then runs it against the current
;;    matcher result."
;;   [f sym]
;;   (matcher
;;    (fn [mr]
;;      (let [v (get-in mr [:vars sym])]
;;        ((f v) mr)))
;;    :multi))

;; ^:rct/test
;; (comment
;;   ;msub substitute :val and :captured
;;   (def ms (msub inc (mval 1)))
;;   (ms {:val 1}) ;=>> {:val 2}
;;   )

;; ;;## Pattern
;; ;;
;; ;; Pattern is a mini language using common clojure data, it can translate to matcher functions.

;; (defn named-var?
;;   "Parse a symbol as a named pattern variable.
;;    Returns `[base-name seq?]` when `v` has the `?x`, `??x` or
;;    forms, otherwise `nil`."
;;   [v]
;;   (when (symbol? v)
;;     (when-let [[_ s n optional?] (re-matches #"(\?[\?]?)([\p{Alnum}-_]+)(\??)" (name v))]
;;       [(when-not (= "_" n) (symbol n)) (cond-> #{} (= optional? "?") (conj :optional) (= s "??") (conj :seq))])))

;; :rct/test
;; (comment
;;   (named-var? '?x3) ;=>>
;;   ['x3 #{}]
;;   (named-var? '?x?) ;=>>
;;   ['x #{:optional}]
;;   (named-var? '?_)
;;   )

;; (defmulti make-matcher
;;   "Compile a low level pattern description like `[:pred pred]` or
;;    `[:seq ...]` into a concrete matcher function."
;;   first)

;; (defn- scalar-element [x]
;;   (cond
;;     (matcher-type x) x
;;     (fn? x) (mpred x)
;;     :else (mval x)))

;; (defn- wrap-element [x]
;;   (let [mt (matcher-type x)] (cond-> x (not mt) scalar-element (not= mt :subseq) mone)))

;; (defn- seq-matcher [x]
;;   (->> (map wrap-element x) (concat [mnone]) mseq))

;; (defn- map-matcher [x]
;;   (->> (map (fn [[k v]] [k (scalar-element v)]) x)
;;        mmap))

;; (defn mscanner
;;   ([children]
;;    (mscanner children nil))
;;   ([children default]
;;    (matcher
;;     (fn [mr]
;;       (reduce (fn [acc mch] (or (mch acc) default acc)) mr children))
;;     :scaner)))

;; (defn vr
;;   [v]
;;   (fn [{vars :vars}]
;;     (walk/postwalk
;;      (fn [x] (or (and (symbol? x) (get vars x)) x))
;;      v)))

;; (def mand #(mscanner % (reduced nil)))

;; (def rule-basic
;;   (msub #(make-matcher (rest %))
;;         (mseq [(mone (mval '?))
;;                (mone (mvar 'type (mpred keyword?)))
;;                (mvar 'args (mrest wildcard))])))

;; (comment
;;   :dbg
;;   ((mseq [(mlazy-subseq wildcard 0 3 false)]) {:val [1 1 1]}))

;; (def rule-named
;;   (msub (fn [sym] (when-let [[var-name flags] (named-var? sym)]
;;                     (cond->> wildcard
;;                       (flags :seq) (#(mrest % 0))
;;                       var-name (mvar var-name)
;;                       (flags :optional) moption)))
;;         (mpred named-var?)))

;; (def rule-seq (msub seq-matcher (mpred #(and (vector? %) (not (map-entry? %))))))
;; (def rule-map (msub map-matcher (mpred map?)))

;; (defn ptn->matcher
;;   ([rules ptn]
;;    (let [srules (mscanner rules)]
;;      (->> ptn
;;           (walk/postwalk
;;            (fn [x] (if-let [mr (srules {:val x})] (:val mr) x)))
;;           scalar-element))))

;; (def matcher-of (partial ptn->matcher [rule-map rule-seq rule-named rule-basic]))

;; ^:rct/test
;; (comment
;;   (map (mscanner [(mseq [(mone (mval 1)) mnone]) (mseq [(mone (mval 2)) mnone])]) [{:val [1]} {:val [2]} {:val [3]}]) ;=>>
;;   [{:val [1]} {:val [2]} {:val [3]}]

;;   ((vr '(* x 5)) {:vars {'x 5}}) ;=>
;;   '(* 5 5)
;;   ((matcher-of '(? :var a (? :val 3))) {:val 3}) ;=>>
;;   {:vars {'a 3}}
;;   )

;; (defmethod make-matcher :pred [[_ pred]] (mpred pred))
;; (defmethod make-matcher :val [[_ v]] (mval v))
;; (defmethod make-matcher :var [[_ sym child]] (mvar sym child))
;; (defmethod make-matcher :seq [[_ & children]] (mseq children))
;; (defmethod make-matcher :map [[_ & children]] (mmap children))
;; (defmethod make-matcher :* [[&_]] wildcard)

;; (defmethod make-matcher :1 [[_ child]] (mone child))
;; (defmethod make-matcher :? [[_ child]] (moption child))

;; (defmethod make-matcher :sub [[_ f child]] (msub f child))
;; (defmethod make-matcher :or [[_ & conditions]] (mor conditions))
;; (defmethod make-matcher :and [[_ & children]] (mand children))
;; (defmethod make-matcher :rest [[_ child]] (mrest (or child wildcard)))
