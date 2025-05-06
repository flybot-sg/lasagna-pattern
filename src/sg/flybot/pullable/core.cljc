(ns sg.flybot.pullable.core)

(defn pred-matcher
  [pred]
  (fn [mr]
    (when (pred (:val mr))
      mr)))

(defn val-matcher [v] (pred-matcher #(= v %)))

(def wildcard (pred-matcher (constantly true)))

(defn var-matcher
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
  ((pred-matcher even?) {:val 4}) ;=> {:val 4}
  ((pred-matcher even?) {:val 3}) ;=> nil
  ((val-matcher 3) {:val 3}) ;=> {:val 3}
  (wildcard {:val 3}) ;=> {:val 3}

  ((var-matcher 'a (val-matcher 3)) {:val 3}) ;=>
  {:val 3 :vars {'a 3}}
  ((var-matcher 'a (val-matcher 3)) {:val 3 :vars {'a 3}}) ;=>>
  {:val 3}
  ((var-matcher 'a (val-matcher 3)) {:val 3 :vars {'a 2}}) ;=> nil
  )

(defn map-matcher
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
  (def mm (map-matcher [[:a (var-matcher 'a (val-matcher 4))] [:b (pred-matcher even?)]]))
  (mm {:val {:a 4 :b 0}})  ;=>
  {:val {:a 4 :b 0} :vars {'a 4}}
  (mm {:val {:a 4 :b 3}}) ;=> nil
  (mm {:val {:a 4 :b 0} :vars {'a 0}}) ;=> nil
  )
