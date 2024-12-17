(ns sg.flybot.pullable.core2
  "Pull pattern for Clojure/script."
  (:require
   [clojure.zip :as zip])
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
  at the end, stays there."
  [loc]
  (cond
    (= :end (loc 1)) [loc nil]

    (and (zip/branch? loc) (zip/down loc)) [(zip/down loc) [:down]]
    (zip/right loc) [(zip/right loc) [:right]]

    :else
    (loop [p loc d []]
      (if (zip/up p)
        (if (zip/right (zip/up p))
          [(zip/right (zip/up p)) (-> d (conj :up) (conj :right))]
          (recur (zip/up p) (conj d :up)))
        [[(zip/node p) :end]]))))

(defn loc-dir-seq
  "use zip-next to generate a sequence of locs and directions"
  [loc]
  (->> (iterate (fn [[l _]] (let [[new-l ds] (zip-next l)] [new-l ds])) [loc []])
       (take-while #(not (zip/end? (first %))))))

(defn either
  "returns a predicate that accepts either `preds`"
  [& preds]
  (fn [x] (reduce #(or %1 (%2 x)) false preds)))

(defn elem-dir
  "returns a sequence of elements and directions from a `seq-loc-dir`"
  [seq-loc-dir]
  (map (fn [[l d]] [(zip/node l) d]) seq-loc-dir))

(defn dir-seq
  "returns a sequence of directions from a `seq-loc-dir`"
  [seq-loc-dir]
  (map second seq-loc-dir))

^:rct/test
(comment
  (-> (zip/vector-zip [1 2 [3 4] 5]) (loc-dir-seq) elem-dir) ;=> 
  [[[1 2 [3 4] 5] []] [1 [:down]] [2 [:right]] [[3 4] [:right]] [3 [:down]] [4 [:right]] [5 [:up :right]]])

(def parent-node? (either sequential? map? set?))

(defn comm-zip
  "returns a common zipper support sequence, maps (as sequence of MapEntry)"
  [data]
  (zip/zipper
   coll?
   identity
   (fn [node children]
     (if (map-entry? node)
       (MapEntry. (first children) (second children)) 
       (-> (into (empty node) children)
           (with-meta (meta node)))))
   data))

^:rct/test
(comment
  (-> (comm-zip [1 2 (array-map :a 3 :b 4) '(5 6) (sorted-set 7 8)])
      (loc-dir-seq)
      (elem-dir)
      vec) ;=>
  [[[1 2 {:a 3, :b 4} [5 6] #{7 8}] []] [1 [:down]] [2 [:right]] [{:a 3, :b 4} [:right]] [[:a 3] [:down]] [:a [:down]] [3 [:right]] [[:b 4] [:up :right]]
   [:b [:down]] [4 [:right]] [[5 6] [:up :up :right]] [5 [:down]] [6 [:right]]
   [#{7 8} [:up :right]] [7 [:down]] [8 [:right]]]
  )

;;----------------
;; matching result (a.k.a mr)

(defn data->mr
  "turns normal data to a mr"
  [data]
  {:loc (comm-zip data)})

(defn mr->data
  "returns data from a mr"
  [mr]
  (when-let [v (some-> mr :loc)]
    (assoc (:vars mr) '& (zip/root v))))

^:rct/test
(comment
  (-> 5 data->mr mr->data) ;=> {& 5}
  (-> nil data->mr mr->data) ;=> {& nil}
  )

(defn run-query
  [q data]
  (-> data data->mr q mr->data))

(def ^:private moves {:down zip/down :right zip/right :up zip/up})

(defn- zip-edit
  [loc f]
  (let [v (zip/node loc)
        new-v (f v)]
    (if (not= v new-v)
      (zip/replace loc new-v)
      loc)))

;; a matcher is a function take a loc, returns a loc
(defn- literal-matcher
  [val]
  (fn [loc]
    (when (= val (zip/node loc))
      loc)))

(defn- map-matcher
  [m]
  (fn [loc]
    (zip/edit loc #(select-keys % (keys m)))))

^:rct/test
(comment
  ((map-matcher {:a 1 :b 2}) [{:a 1 :b 2 :c 3} nil]) ;=>
  [{:a 1 :b 2} {:changed? true}]
  )

(defn- matcher-of
  [f dirs]
  (fn [mr]
    (let [loc (:loc mr)
          new-loc (-> (reduce #((moves %2) %1) loc dirs)
                      (f))]
      (when new-loc (assoc mr :loc new-loc)))))

(defn- with-move
  [matcher dirs]
  (fn [mr]
    (let [loc (:loc mr)
          new-loc (-> (reduce #((moves %2) %1) loc dirs)
                      (matcher))]
      (when new-loc (assoc mr :loc new-loc)))))

(defn- element->fun
  "returns a function which takes a loc and dirs, returns a function which takes a mr returns a new mr"
  [[loc dirs]]
  (fn [mr]
    (let [val (zip/node loc)
          matcher (cond
                    (sequential? val) (with-move identity dirs)
                    (map? val) (with-move (map-matcher val) dirs)
                    :else
                    (with-move (literal-matcher val) dirs))]
      (matcher mr))))

^:rct/test
(comment
  ((element->fun [[[1 2] nil] []]) {:loc [[1 2] nil]}) ;=> 
  {:loc [[1 2] nil]}
  )

(defn compile-pattern
  "returns a sequence of functions which takes a mr returns a new mr"
  [pattern]
  (let [fs (->> (comm-zip pattern)
                (loc-dir-seq)
                (map element->fun))]
    (fn [mr]
      (reduce #(or (%2 %1) (reduced nil)) mr fs))))

^:rct/test
(comment
  (run-query (compile-pattern 5) 5) ;=> {& 5}
  (run-query (compile-pattern 5) 3) ;=> nil 
  (run-query (compile-pattern '[1 2]) [1 2]) ;=> {& [1 2]}
  (run-query (compile-pattern '[1 2]) [2 3]) ;=> nil
  (run-query (compile-pattern '{:a 1}) {:a 1 :b 2}) ;=> {& {:a 1}}
  (run-query (compile-pattern '[1 (2 3) #{4} {5 6}]) '[1 (2 3) #{4} {5 6}]) ;=> {& [1 (2 3) #{4} {5 6}]}
  (run-query (compile-pattern '[1 {:b {:c 3}}]) [1 {:a 1 :b {:c 3 :d 4}}]) ;=> {& [1 {:b {:c 3}}]}
  )