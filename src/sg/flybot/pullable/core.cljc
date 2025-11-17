(ns sg.flybot.pullable.core
  (:require [clojure.walk :as walk]))

(defn merge-mr
  [mr1 mr2]
  (let [sconj (fn [v1 v2] ((if (vector? v2) into conj) (if (vector? v1) v1 [v1]) v2))
        update-if (fn [m k f & args] (let [v (apply f (get m k) args)]
                                       (if (nil? v) m (assoc m k v))))]
    (-> mr1
        (update-if :val (fn [v1] (let [v2 (:val mr2)]
                                   (if (and (map? v1) (map-entry? v2)) (conj v1 v2) v2))))
        (update-if :vars #(merge-with sconj % (:vars mr2))))))

^:rct/test
(comment
  (merge-mr {:val 3} {:val 4}) ;=> {:val 4}
  (merge-mr {:val {:a 1}} (:val (first {:b 2})))
  (= {:vars {'a [1 2] 'b 2}}
   (merge-mr {:vars {'a 1 'b 2}} {:vars {'a 2}})
   (merge-mr {:vars {'a [1]}} {:vars {'a 2 'b 2}})
   (merge-mr {:vars {'a 1 'b 2}} {:vars {'a [2]}})) ;=> true
  )

(defn matcher [f t & {:as more}] (with-meta f (merge more {::matcher-type t})))

(defn matcher?
  [x]
  (and (fn? x) (some-> x meta ::matcher-type)))

(defn mpred
  [pred]
  (matcher
   (fn [mr]
     (when (pred (:val mr))
       mr))
   :pred))

(defn mval [v] (matcher (mpred #(= v %)) :val ::value v))
(defn value-of [v] (if (matcher? v) (some-> v meta ::value) v))

(def wildcard (mpred (constantly true)))

(defn mvar
  [sym child]
  (matcher
   (fn [mr]
     (when-let [mr (child mr)]
       (let [new-v (or (:captured mr) (:val mr))
             old-v (get-in mr [:vars sym] ::not-found)
             mr (dissoc mr :captured)]
         (cond
           (= old-v ::not-found) (assoc-in mr [:vars sym] new-v)
           (= old-v new-v) mr
           :else nil))))
   :var ::symbol sym))

^:rct/test
(comment
  ((mpred even?) {:val 4}) ;=>> {:val 4}
  ((mpred even?) {:val 3}) ;=> nil
  ((mval 3) {:val 3}) ;=>> {:val 3}
  (wildcard {:val 3}) ;=>> {:val 3}

  ((mvar 'a (mval 3)) {:val 3}) ;=>
  {:val 3 :vars {'a 3}}
  ((mvar 'a (mval 3)) {:val 3 :vars {'a 3}}) ;=>>
  {:val 3}
  ((mvar 'a (mval 3)) {:val 3 :vars {'a 2}}) ;=> nil
  )

(defn mmap
  [k-matchers]
  (matcher
   (fn [mr]
     (reduce
      (fn [mr' [k mch]]
        (if-let [vmr (update mr' :val get k)]
          (if-let [vmr (mch vmr)]
            (-> mr'
                (update :val conj [k (:val vmr)])
                (update :vars merge (:vars vmr)))
            (reduced nil))
          (reduced nil)))
      mr k-matchers))
   :map))

^:rct/test
(comment
  (def mm (mmap [[:a (mvar 'a (mval 4))] [:b (mpred even?)]]))
  (mm {:val {:a 4 :b 0}})  ;=>
  {:val {:a 4 :b 0} :vars {'a 4}}
  (mm {:val {:a 4 :b 3}}) ;=> nil
  (mm {:val {:a 4 :b 0} :vars {'a 0}}) ;=> nil
  )

(defn mone
  [child]
  (matcher
   (fn [mr]
     (when-let [mr' (child (assoc mr :val (-> mr :rest first)))]
       (some-> mr (update :vars merge (:vars mr')) (update :rest rest))))
   :subseq ::length [1 1]))

(defn mrest
  [child]
  (matcher
   (fn [mr]
     (let [melems (->> (:rest mr) (map #(child (assoc mr :val %))) (take-while identity))]
       (-> mr
           (assoc :rest (->> (:rest mr) (drop (count melems))))
           (assoc :captured (->> melems (map :val) (reduce conj []))))))
   :subseq ::length [0]))

(defn msubseq
  [& matchers]
  (matcher
   (fn [mr]
     (reduce (fn [mr' mch] (or (mch mr') (reduced nil))) mr matchers))
   :subseq))

(defn mseq
  [matchers]
  (matcher
   (fn [mr]
     (let [mr (reduce (fn [mr' mch]
                        (or
                         (mch mr')
                         (reduced nil)))
                      (assoc mr :rest (:val mr))
                      matchers)]
       (when-not (some-> mr :rest seq)
         (dissoc mr :rest))))
   :seq))

^:rct/test
(comment
  ;mone matches a single sequence value
  (def mo (mone (mvar 'a (mval 1))))
  (mo {:val [1] :rest [1]}) ;=>> {:vars {'a 1} :rest []}
  (mo {:val [2] :rest [2]}) ;=> nil

  (def mr (mvar 'a (mrest (mpred even?))))
  (mr {:val [2 4 6 1] :rest [2 4 6 1]}) ;=>> {:vars {'a [2 4 6]} :rest [1]}
  (mr {:val [2 0] :rest [2 0] :vars {'b -1}})

  ;mseq matches a whole sequence
  (def sm (mseq [mo (mone (mpred zero?)) (mone (mpred odd?))]))
  (sm {:val [1 0 3]}) ;=>> {:vars {'a 1}}
  (sm {:val [1 0 3 2]}) ;=> nil

  (def sm (mseq [(mone (mvar 'b (mval 1))) (mone (mpred zero?)) (mvar 'a (mrest (mpred odd?)))]))
  (sm {:val [1 0 1 3 5]}) ;=>> {:vars {'a [1 3 5]}}
  (sm {:val [1 0 1 3 5 2]}) ;=> nil

  (def sm2 (mseq [(mone (mvar 'b (mval 1))) (msubseq (mone (mpred zero?)))]))
  (sm2 {:val [1 0]}))

(defn msub
  [f child]
  (matcher
   (fn [mr]
     (when-let [mr (child mr)]
       (let [v (f (:val mr))]
         (assoc mr :val v))))
   :sub))

(defn mor
  [& kms]
  (let [km-pair (partition 2 kms)]
    (matcher
     (fn [mr]
       (when-let [[t mr'] (->> km-pair
                               (map (fn [[k mch]] [k (mch mr)]))
                               (filter second)
                               first)]
         (assoc mr' :captured t)))
     :or)))

(defn mwalk
  [& pms]
  (let [pm-pair (partition 2 pms)]
    (matcher
     (fn [mr]
       (update mr :val
               (fn [v]
                 (walk/postwalk
                  (fn [x]
                    (or
                     (->> pm-pair
                          (map (fn [[pred mch]] (when (pred x) (:val (mch (assoc mr :val x))))))
                          (filter identity)
                          first)
                     x))
                  v))))
     :walk)))

(defn mmulti
  [f sym]
  (matcher
   (fn [mr]
     (let [v (get-in mr [:vars sym])]
       ((f v) mr)))
   :multi))

^:rct/test
(comment
  ;msub substitute :val and :captured
  (def ms (msub inc (mval 1)))
  (ms {:val 1}) ;=>> {:val 2}

  ;mor find the first matched branch of patterns and capture the type
  (def mo (mvar 'type (mor :keyword (mpred keyword?)
                           :number (msub str (mpred number?)))))
  (mo {:val 3}) ;=>> {:vars {'type :number}}

  (def mw (mwalk list? (msub #(apply * %) (mseq [(mone wildcard) (mone (mpred number?))]))
                 number? (msub #(* 2 %) (mpred number?))))
  (mw {:val '(2 5)}) ;=>> {:val 40}
  (mw {:val '(4 (2 5))}) ;=>> {:val 320}
  )

(defmulti make-matcher first)
(defmethod make-matcher :pred [[_ pred]] (mpred pred))
(defmethod make-matcher :val [[_ v]] (mval v))
(defmethod make-matcher :var [[_ sym child]] (mvar sym child))
(defmethod make-matcher :seq [[_ & children]] (mseq (map #(if (= (-> % meta ::matcher-type) :subseq) % (mone %)) children)))
(defmethod make-matcher :map [[_ & kvs]] (mmap (partition 2 kvs)))

(defmulti matcher-args identity)
(defmethod matcher-args :default [_] (mrest wildcard))
(defmethod matcher-args :seq [_] (mrest (mpred matcher?)))
(defmethod matcher-args :pred [_] (mone (mpred ifn?)))
(defmethod matcher-args :val [_] (mone wildcard))
(defmethod matcher-args :var [_] (msubseq (mone (mpred symbol?)) (mone (mpred matcher?))))

(defn named-var?
  [v]
  (and (symbol? v) (re-matches #"\?([a-zA-Z][a-zA-Z0-9_-]*)" (name v))))

(comment
  (named-var? '?x3)
  )

(defn ptn->fn
  [ptn]
  (let [mf (mwalk
            list?
            (msub (fn [v] (make-matcher (rest v)))
                  (mseq [(mone (mval '?)) (mone (mvar 'type (mpred keyword?))) (mmulti matcher-args 'type)]))

            map?
            (msub (fn [v] (mmap v)) wildcard)

            (fn [x] (and (vector? x) (not (map-entry? x))))
            (msub (fn [v] (make-matcher (cons :seq v))) wildcard)

            #(= % '?_)
            (msub (constantly wildcard) wildcard)

            named-var?
            (msub (fn [v] (let [[_ var-name] (named-var? v)] (mvar (symbol var-name) wildcard))) wildcard))
        rslt (mf {:val ptn})]
    (when rslt (:val rslt))))

^:rct/test
(comment
                                        ;introduce pattern
  (def pf (ptn->fn '(? :var a (? :val 20))))
  (pf {:val 20})                        ;=>> {:val 20}
  (def pf2 (ptn->fn '(? :seq (? :var a (? :val 20)))))
  (pf2 {:val [20]})
  (def pf3 (ptn->fn '(? :map :a (? :var a (? :val 20)))))
  (pf3 {:val {:a 20}})
  ((ptn->fn '?_) {:val 23})             ;=>> {:val 23}
  ((ptn->fn '[?_ ?_]) {:val [2 1]})     ;=>> {:val [2 1]}
  ((ptn->fn '[?a ?b]) {:val [3 4]})     ;=>> {:vars '{a 3 b 4}}
  ((ptn->fn '{:a ?a}) {:val {:a 5}})    ;=>> {:vars '{a 5}}
  )
