(ns sg.flybot.pullable.core)

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
      (let [new-v (:val mr)
            old-v (get-in mr [:vars sym] ::not-found)]
        (cond
          (= old-v ::not-found) (assoc-in mr [:vars sym] new-v)
          (= old-v new-v) mr
          :else nil)))))

^:rct/test
(comment
  ((mpred even?) {:val 4}) ;=> {:val 4}
  ((mpred even?) {:val 3}) ;=> nil
  ((mval 3) {:val 3}) ;=> {:val 3}
  (wildcard {:val 3}) ;=> {:val 3}

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

(defn mseq
  [matchers]
  (fn [mr]
    (some->> matchers
             (reduce (fn [mr' mch]
                       (or
                        (mch mr')
                        (reduced nil)))
                     (assoc mr :rest (:val mr)))
             )))

^:rct/test
(comment
  (def mo (mone (mvar 'a (mval 1))))
  (mo {:val [1] :rest [1]}) ;=>> {:vars {'a 1} :rest []}
  (mo {:val [2] :rest [2]}) ;=> nil
  (def sm (mseq [mo (mone (mpred zero?)) (mone (mpred odd?))]))
  (sm {:val [1 0 3]}))
