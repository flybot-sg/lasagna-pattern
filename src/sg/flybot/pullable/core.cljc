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

(defn mpred
  [pred]
  (fn [mr]
    (when (pred (:val mr))
      mr)))

(defn mval [v] (mpred #(= v %)))

(def wildcard (mpred (constantly true)))

(defn mvar
  [sym child]
  (fn [mr]
    (when-let [mr (child mr)]
      (let [new-v (or (:captured mr) (:val mr))
            old-v (get-in mr [:vars sym] ::not-found)
            mr (dissoc mr :captured)]
        (cond
          (= old-v ::not-found) (assoc-in mr [:vars sym] new-v)
          (= old-v new-v) mr
          :else nil)))))

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
     mr k-matchers)))

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
  (fn [mr]
    (when-let [mr' (child (assoc mr :val (-> mr :rest first)))]
      (some-> mr (update :vars merge (:vars mr')) (update :rest rest)))))

(defn mrest
  [child]
  (fn [mr]
    (let [melems (->> (:rest mr) (map #(child (assoc mr :val %))) (take-while identity))]
      (-> mr
          (assoc :rest (->> (:rest mr) (drop (count melems))))
          (assoc :captured (->> melems (map :val) (reduce conj [])))))))

(defn mseq
  [matchers]
  (fn [mr]
    (let [mr (reduce (fn [mr' mch]
                    (or
                     (mch mr')
                     (reduced nil)))
                  (assoc mr :rest (:val mr))
                  matchers)]
      (when-not (some-> mr :rest seq)
        (dissoc mr :rest)))))

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
  )

(defn msub
  [f child]
  (fn [mr]
    (when-let [mr (child mr)]
      (let [v (f (:val mr))]
        (assoc mr :val v)))))

(defn mor
  [& kms]
  (let [km-pair (partition 2 kms)]
    (fn [mr]
      (when-let [[t mr'] (->> km-pair
                              (map (fn [[k mch]] [k (mch mr)]))
                              (filter second)
                              first)]
        (assoc mr' :captured t)))))

(defn mwalk
  [& pms]
  (let [pm-pair (partition 2 pms)]
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
                 v))))))

^:rct/test
(comment
  ;msub substitute :val and :captured
  (def ms (msub inc (mval 1)))
  (ms {:val 1}) ;=>> {:val 2}

  ;mor find the first matched branch of patterns and capture the type
  (def mo (mvar 'type (mor :keyword (mpred keyword?)
                           :number (msub str (mpred number?)))))
  (mo {:val 3}) ;=>> {:vars {'type :number}}

  (def mw (mwalk list? (msub #(apply * %) (mseq [(mone wildcard) (mone (mpred number?))]))))
  (mw {:val '(2 5)}) ;=>> {:val 10}
  (mw {:val '(4 (2 5))}) ;=>> {:val 40}
  )

(defmulti make-matcher first)
(defmethod make-matcher :pred [[_ pred]] (mpred pred))
(defmethod make-matcher :val [[_ v]] (mval v))
(defmethod make-matcher :var [[_ sym child]] (mvar sym child))

(defn ptn->fn
  [ptn]
  (let [mf (mwalk list? (msub (fn [v] (make-matcher (rest v)))
                              (mseq [(mone (mval '?)) (mone (mvar 'type (mpred keyword?))) (mrest wildcard)])))
        rslt (mf {:val ptn})]
    (when rslt
      (with-meta (:val rslt) {:matcher/type (get-in rslt [:vars 'type])}))))

^:rct/test
(comment
  ;introduce pattern
  (def pf (ptn->fn '(? :var a (? :val 20))))
  (pf {:val 20}) ;=>> {:val 20}
  )
