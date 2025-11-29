(ns sg.flybot.pullable.core
  (:require [clojure.walk :as walk]))

(defn matcher [f t & {:as more}] (with-meta f (merge more {::matcher-type t})))
(defn matcher-type
  [x]
  (and (fn? x) (some-> x meta ::matcher-type)))

(defn mpred
  [pred]
  (matcher
   (fn [mr]
     (when (pred (:val mr))
       mr))
   :pred))

(defn mval [v] (mpred #(= v %)))

(def wildcard (mpred (constantly true)))

(defn mvar
  [sym child]
  (matcher
   (fn [mr]
     (when-let [mr (child mr)]
       (let [new-v (or (:captured mr) (:val mr))
             old-v (get-in mr [:vars sym] ::not-found)
             mr (dissoc mr :captured)]
         (condp = old-v
           ::not-found (assoc-in mr [:vars sym] new-v)
           new-v mr
           nil))))
   (matcher-type child) ::symbol sym))

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
  ((mvar 'a (mval 3)) {:val 3 :vars {'a 2}})) ;=> nil

(defn mmap
  [k-matchers]
  (matcher
   (fn [mr]
     (reduce
      (fn [mr' [k mch]]
        (if-let [vmr (some-> (update mr' :val get k) (mch))]
          (-> mr'
                (update :val conj [k (:val vmr)])
                (update :vars merge (:vars vmr)))
          (reduced nil)))
      mr k-matchers))
   :map))

^:rct/test
(comment
  (def mm (mmap [[:a (mvar 'a (mval 4))] [:b (mpred even?)]]))
  (mm {:val {:a 4 :b 0}})  ;=>
  {:val {:a 4 :b 0} :vars {'a 4}}
  (mm {:val {:a 4 :b 3}}) ;=> nil
  (mm {:val {:a 4 :b 0} :vars {'a 0}})) ;=> nil
  

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

(def find-first (comp first filter))

(defn mor
  [kms]
  (let [km-pair (partition 2 kms)]
    (matcher
     (fn [mr]
       (when-let [[t mr'] (->> km-pair
                               (map (fn [[k mch]] [k (mch mr)]))
                               (find-first second))]
         (assoc mr' :captured t)))
     :or)))

(defn mwalk
  [final-f & pms]
  (let [pm-pair (partition 2 pms)]
    (matcher
     (fn [mr]
       (update mr :val
               (fn [v]
                 (final-f
                  (walk/postwalk
                   (fn [x]
                     (or
                      (->> pm-pair
                           (map (fn [[pred mch]] (when (pred x) (:val (mch (assoc mr :val x))))))
                           (find-first identity))
                      x))
                   v)))))
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
  (def mo (mvar 'type (mor [:keyword (mpred keyword?)
                            :number (msub str (mpred number?))])))
  (mo {:val 3}) ;=>> {:vars {'type :number}}

  (def mw (mwalk
           identity
           list? (msub #(apply * %) (mseq [(mone wildcard) (mone (mpred number?))]))
           number? (msub #(* 2 %) (mpred number?))))
  (mw {:val '(2 5)}) ;=>> {:val 40}
  (mw {:val '(4 (2 5))})) ;=>> {:val 320}
  

(defmulti make-matcher first)
(defmethod make-matcher :pred [[_ pred]] (mpred pred))
(defmethod make-matcher :val [[_ v]] (mval v))
(defmethod make-matcher :var [[_ sym child]] (mvar sym child))
(defn wrap-element
  ([item]
   (let [mt (matcher-type item)] (cond-> item (false? mt) mval (not= mt :subseq) mone))))

(defmethod make-matcher :seq [[_ & children]]
  (mseq (map wrap-element children)))
(defmethod make-matcher :map [[_ & kvs]]
  (mmap (map (fn [[k mch]] [k (cond-> mch (not (matcher-type mch)) mval)]) (partition 2 kvs))))
(defmethod make-matcher :sub [[_ f child]] (msub f child))
(defmethod make-matcher :or [[_ & conditions]] (mor conditions))
(defmethod make-matcher :rest [[_ child]] (mrest (or child wildcard)))

(defmulti matcher-args identity)
(defmethod matcher-args :default [_] (mrest wildcard))
(defmethod matcher-args :seq [_] (mrest (mpred matcher-type)))
(defmethod matcher-args :pred [_] (mone (mpred ifn?)))
(defmethod matcher-args :val [_] (mone wildcard))
(defmethod matcher-args :var [_] (msubseq (mone (mpred symbol?)) (mone (mpred matcher-type))))
(defmethod matcher-args :sub [_] (msubseq (mone (mpred ifn?)) (mone (mpred matcher-type))))

(defn named-var?
  [v]
  (when (symbol? v)
    (when-let [[_ s n] (re-matches #"(\?[\?]?)([a-zA-Z][a-zA-Z0-9_-]*)" (name v))]
      [(symbol n) (= s "??")])))

^:rct/test
(comment
  (named-var? '?x3) ;=>> ['x3 false]
  (named-var? '??x)) ;=>> ['x true]
  

(defn ptn->fn
  "compiles a ptn syntax to a function matches any data, returns a matching result"
  [ptn]
  (let [var? #{'? '??}
        mf (mwalk
            ;;if returns plan value, make it a val matcher
            (fn [v] (if (matcher-type v) v (mval v)))

            ;; support general matcher syntax
            (fn [v] (and (list? v) (var? (first v))))
            (msub (fn [v] (cond-> (make-matcher (rest v)) (= '?? (first v)) mrest))
                  (mseq [(mone (mpred var?)) (mone (mvar 'type (mpred keyword?))) (mmulti matcher-args 'type)]))

            ;; map is a shortcut for :map matcher
            map?
            (msub (fn [v] (make-matcher (cons :map (flatten (vec v))))) wildcard)

            ;; vector is a shortcut for :seq matcher
            (fn [x] (and (vector? x) (not (map-entry? x))))
            (msub (fn [v] (make-matcher (cons :seq v))) wildcard)

            ;; wildcard is ?_
            #(= % '?_)
            (msub (constantly wildcard) wildcard)

            ;; named var is a shortcut for var wildcard
            named-var?
            (msub (fn [v] (let [[var-name s?] (named-var? v)]
                            (mvar var-name (cond-> wildcard s? mrest))))
                  wildcard))
        rslt (mf {:val ptn})]
    (fn [data]
      (when rslt ((:val rslt) {:val data})))))

^:rct/test
(comment
  ((ptn->fn '(? :var a (? :val 20))) 20)                        ;=>> {:val 20}
  ((ptn->fn '[(? :var a (?? :val 20))]) [20 20]) ;=>> {:vars {'a [20 20]}}
  ((ptn->fn '(? :seq (? :var a (? :val 20)))) [20]) ;=>> {:val [20] :vars {'a 20}}
  ((ptn->fn '(? :map :a (? :var a (? :val 20)))) {:a 20}) ;=>> {:val {:a 20} :vars {'a 20}}
  ((ptn->fn '?_) 23)             ;=>> {:val 23}
  ((ptn->fn '[?_ ?_]) [2 1])     ;=>> {:val [2 1]}
  ((ptn->fn '[?a ?b]) [3 4])     ;=>> {:vars '{a 3 b 4}}
  ((ptn->fn '{:a ?a :b 3}) {:a 5 :b 3})    ;=>> {:vars '{a 5}}
  ((ptn->fn '[5 ?a]) [5 -1])     ;=>> {:vars '{a -1}}
  ((ptn->fn 5) 5) ;=> {:val 5}
  ((ptn->fn (list '? :sub #(* % %) (list '? :pred number?))) 8) ;=> {:val 64}
  ((ptn->fn (list '? :pred odd?)) 3) ;=> {:val 3}
  ((ptn->fn (list '? :var 'type (list '? :or :number (list '? :pred number?) :string (list '? :pred string?)))) 3) ;=>>
  {:val 3 :vars {'type :number}}
  ((mseq [(mone (mval 1)) (mone (mval 2)) (mone (mval 3)) (mvar 'a (mrest wildcard))]) {:val [1 2 3 4 5]})
  (= (mseq [(mone (mval 1)) (mone (mval 2)) (mone (mval 3)) (mvar 'a (mrest wildcard))]) (ptn->fn '[1 2 3 ??a]))
  ((ptn->fn '[1 2 3 ??end]) [1 2 3 4 5]) ;=>> {:vars {'end [4 5]}}

  ;; complex pattern unification
  (mapv (ptn->fn '{:a ?x :b {:c ?x}}) [{:a 3 :b {:c 3}} {:a 2 :b {:c 3}}]) ;=>>
  [{:vars {'x 3}} nil]
  (mapv (ptn->fn '[?x 5 ?_ {:a ?x}]) [[3 5 nil {:a 3}] [3 1]]) ;=>>
  [{:vars {'x 3}} nil])
