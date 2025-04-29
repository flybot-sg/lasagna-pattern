(ns sg.flybot.pullable.core2
  "Pull pattern for Clojure/script."
  (:require
   [clojure.walk :as walk]
   [clojure.zip :as zip]
   [sg.flybot.pullable.util :refer [cond-let either]])
  (:import
   [clojure.lang MapEntry]))

;;----------------
;; zip utilities

(defn zip-next
  "Modified version of zip/next, returns a tuple [loc direction],
  where loc is the next loc in the hierarchy, and direction is a array of
  keyword of `:down`, `:right`, `:up`.

  Moves to the next loc in the hierarchy, depth-first. When reaching
  the end, returns a distinguished loc detectable via end?. If already
  at the end, returns nil."
  [loc]
  (cond-let
   [_ (zip/end? loc)] [loc nil]

   [l (and (zip/branch? loc) (zip/down loc))] [l [:down]]

   [_ (zip/right loc)] [(zip/right loc) [:right]]

   (loop [p loc d []]
     (if-let [up (zip/up p)]
       (if-let [right-up (zip/right up)]
         [right-up (-> d (conj :up) (conj :right))]
         (recur up (conj d :up)))
       [[(zip/node p) :end] d]))))

(defn data-zip
  "returns a common data zipper support sequence, maps (as sequence of MapEntry)"
  [data]
  (zip/zipper
   coll?
   seq
   (fn [node children]
     (if (map-entry? node)
       (MapEntry. (first children) (second children)) ;has to patch for a map-entry
       (-> (into (empty node) children)
           (with-meta (meta node)))))
   data))

(defn zip-visit
  "visit zip `node` returns a lazy sequence of pairs of node data and directions to the node"
  [node]
  (->> [node]
       (iterate (fn [[loc]] (zip-next loc)))
       (eduction (comp (halt-when (fn [[loc]] (zip/end? loc)))
                       (map (fn [[loc dirs]] [(zip/node loc) dirs]))))))

^:rct/test
(comment
  ;data zip
  (zip-visit (data-zip [1 2 (array-map :a 3 :b 4) '(5 6) (sorted-set 7 8)])) ;=>
  [[[1 2 {:a 3, :b 4} [5 6] #{7 8}] nil] [1 [:down]] [2 [:right]] [{:a 3, :b 4} [:right]] [[:a 3] [:down]] [:a [:down]] [3 [:right]] [[:b 4] [:up :right]]
   [:b [:down]] [4 [:right]] [[5 6] [:up :up :right]] [5 [:down]] [6 [:right]]
   [#{7 8} [:up :right]] [7 [:down]] [8 [:right]]])

(defn zip-root
  "copy and modified from `clojure.zip/root`, merge the orignal map data in meta."
  [loc]
  (letfn [(merged
            [n]
            (if-let [orig (and (map? n) (some-> n meta ::orig-map))]
              (merge orig n)
              n))]
    (if (zip/end? loc)
      (merged (zip/node loc))
      (if-let [p (some-> loc (zip/up) (zip/edit merged))]
        (recur p)
        (merged (zip/node loc))))))

;;------------------------------
;; ## Matching result (a.k.a mr)
;; Matching result is a context that contains the current state of the matching process.

(defn data->mr
  "turns normal data to a mr"
  [data]
  {:loc (data-zip data)})

(defn mr->data
  "returns data from a mr"
  [mr]
  (when-let [v (some-> mr :loc)]
    (assoc (:vars mr) '& (zip-root v))))

^:rct/test
(comment
  (-> 5 data->mr mr->data) ;=> {& 5}
  (-> nil data->mr mr->data))  ;=> {& nil}

;;----------------

(defn comp-matchers
  "returns a matcher that composes a list of matchers and dirs pairs"
  [matcher-dir-pairs]
  (fn [mr _]
    (reduce (fn [mr [matcher dirs]] (or (matcher mr dirs) (reduced nil))) mr matcher-dir-pairs)))

(defn apply-ms
  "apply a matcher to data, returns a mr or nil if no match"
  [f data]
  (-> (data->mr data)
      (f nil)
      (mr->data)))

(defn move
  "move `mr` in `dirs`, return a new mr"
  [mr dirs]
  (letfn [(moves [dir]
            (get {:down zip/down :right zip/right :up zip/up} dir
                 (constantly (ex-info "Unknown direction" {:dir dir}))))
          (go [loc dir]
            (if (some? loc) ((moves dir) loc) (reduced nil)))]
    (update mr :loc #(reduce go % dirs))))

(defn m-map
  "A structure matcher factory for map data"
  [key-matchers]
  (fn [mr dirs]
    (letfn [(replace
              [[n p :as loc]]
              (with-meta
                [(-> (select-keys n (map #(% mr) key-matchers))
                     (with-meta (assoc (meta n) ::orig-map n)))
                 (assoc p :changed? true)]
                (meta loc)))]
      (-> mr (move dirs) (update :loc replace)))))

^:rct/test
(comment
  ;;m-map 
  (apply-ms (comp-matchers [[(m-map [(constantly :a) (constantly :b)])]
                            [(m-lvar 'k (m-wildcard)) [:down :down]]
                            [(m-lvar 'v (m-wildcard)) [:right]]])
            {:a 1 :b 2 :c 3})  ;=> {& {:a 1, :b 2, :c 3}, k :a, v 1}
  )

(defn pattern-zip
  "returns a zipper for the pattern"
  [pattern]
  (zip/zipper
   (either vector? map?)
   vec
   (fn [node children]
     (with-meta (vec children) (meta node)))
   pattern))

(defn m-noop
  [mr dirs]
  (move mr dirs))

(defn elem-conv
  [[elem dirs]]
  [(cond
     (map? elem) (m-map (keys elem))
     (nil? dirs) m-noop
     (map-entry? elem) m-noop
     :else elem)
   dirs])

(defn ptn->m
  [pattern]
  (->> pattern pattern-zip zip-visit (map elem-conv) comp-matchers))

;;---------------------
;; ## Matcher factories
;;
;; A matcher is a multi-arity function accept a `mr`, returns a new mr or nil if no match.
;; Matchers also need `dirs` to traverse the target data.
;;  - 1 arity, returns a value if used as a key of a map
;;  - 2 arity, match on a mr and dirs to this pred

(defn m-pred
  "A matcher that matches a predicate on the current node."
  ([pred]
   (m-pred pred nil))
  ([pred kf]
   (fn
     ([mr] (or (kf mr) (throw (ex-info "no key specified for m-pred" {}))))
     ([mr dirs]
      (let [mr (move mr dirs)
            v (some-> mr :loc zip/node)]
        (when (pred v) mr))))))

(defn m-val
  "a matcher can match a value `v`"
  [v]
  (m-pred #(= % v) (constantly v)))

(defn m-wildcard
  "a wildcard matcher, always returns true"
  []
  (m-pred (constantly true)))

^:rct/test
(comment
  ;;common m-pred
  ((m-pred even?) {:loc [4 nil]} nil) ;=> {:loc [4 nil]}
  ((m-pred even?) {:loc [3 nil]} nil) ;=> nil
  ;;apply-ms
  (apply-ms (m-pred even?) 3) ;=> nil
  ;;comp-matchers
  (apply-ms (m-wildcard) 4)  ;=> {& 4}
  (apply-ms (ptn->m [(m-wildcard) {(m-val :b) (m-lvar 'b (m-wildcard))}])
            [2 {:a 3 :b 4}]) ;=>> '{b 4}
  )

(defn m-lvar
  "A matcher that binds a symbol to a value in the mr"
  ([sym]
   (m-lvar sym (m-wildcard)))
  ([sym m]
   (letfn [(get-v [mr] (get-in mr [:vars sym] ::not-found))]
     (fn
       ([mr] (let [v (get-v mr)] (when-not (= ::not-found v) v)))
       ([mr dirs]
        (when-let [mr' (m mr dirs)]
          (let [old-v (get-v mr')
                v (some-> mr' :loc zip/node)]
            (if (= old-v ::not-found)
              (assoc-in mr' [:vars sym] v)
              (when (= old-v v) mr')))))))))

^:rct/test
(comment
  ;;common m-lvar
  ((m-lvar 'a (m-wildcard)) {:loc [4 nil]} nil) ;=>>
  {:vars {'a 4}}
  ;;m-lvar with joining
  ((m-lvar 'a (m-pred even?)) {:loc [4 nil] :vars {'a 3}} nil) ;=> nil
  ((m-lvar 'a (m-pred even?)) {:loc [4 nil] :vars {'a 4}} nil) ;=>>
  {:vars {'a 4}}
  ;;m-lvar as a key
  (apply-ms (ptn->m [(m-lvar 'a (m-val :a)) {(m-lvar 'a) (m-lvar 'b)}])
            [:a {:a 3}]) ;=>
  '{a :a, b 3 & [:a {:a 3}]})

(defn m-term
  "A matcher that matches the end of a sequence"
  []
  (fn [mr dirs]
    (when-not (some-> mr (move dirs) :loc)
      mr)))

^:rct/test
(comment
  ((m-term) {:loc (-> (data-zip [1 2]) zip/down) :vars {}} [:right :right]) ;=>> some?
  ((m-term) {:loc (-> (data-zip [1 2]) zip/down) :vars {}} [:right])  ;=> nil
  )

(defn spec->matchers
  "turns a `spec-vec` vector into a matcher function"
  [coll]
  (walk/postwalk
   (fn [x]
     (if (vector? x)
       (let [[key & args] x
             matcher-types {:wild m-wildcard,
                            :val m-val,
                            :lvar m-lvar,
                            :term m-term,
                            :pred m-pred}
             f (or (get matcher-types key) 
                   (throw (ex-info "unknown key for matcher spec" {:key key})))]
         (apply f args))
       x))
   coll))

^:rct/test
(comment
  ((spec->matchers [:wild]) {:loc [4 nil]} []) ;=>>some?
  ((spec->matchers [:val 3]) {:loc [3 nil]} []) ;=>>some? 
  ((spec->matchers [:pred odd?]) {:loc [3 nil]} []) ;=>>some? 
  (apply-ms (ptn->m [(spec->matchers [:term])]) []) ;=>>some? 
  ((spec->matchers [:lvar 'a [:wild]]) {:loc [3 nil]} []) ;=>>
  {:vars {'a 3}}
  )