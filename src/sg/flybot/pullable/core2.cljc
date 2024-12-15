(ns sg.flybot.pullable.core2
  "Pull pattern for Clojure/script."
  (:require
   [clojure.zip :as zip]))

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
   parent-node?
   seq
   (fn [node children]
     (with-meta children (meta node)))
   data))

^:rct/test
(comment
  (-> (comm-zip [1 2 (array-map :a 3 :b 4) '(5 6) (sorted-set 7 8)])
      (loc-dir-seq)
      (elem-dir)
      vec) ;=>
  [[[1 2 {:a 3, :b 4} [5 6] #{7 8}] []] [1 [:down]] [2 [:right]] [{:a 3, :b 4} [:right]] [[:a 3] [:down]] [:a [:down]] [3 [:right]] [[:b 4] [:up :right]]
   [:b [:down]] [4 [:right]] [[5 6] [:up :up :right]] [5 [:down]] [6 [:right]]
   [#{7 8} [:up :right]] [7 [:down]] [8 [:right]]])

;;----------------
;; matching result (a.k.a mr)

(defn data->mr
  "turns normal data to a mr"
  [data]
  {:loc (comm-zip data)})

(defn mr->data
  "returns data from a mr"
  [mr]
  (when-let [loc (:loc mr)]
    (zip/root loc)))

(comment
  (-> 5 data->mr mr->data) ;=> 5
  )

(defn run-query
  [q data]
  (-> data data->mr q mr->data))

(def ^:private moves {:down zip/down :right zip/right :up zip/up})

(defn- literal-matcher
  [val]
  (fn [data-val]
    (when (= val data-val)
      data-val)))

(defn- zip-edit
  [loc f]
  (let [v (zip/node loc)
        new-v (f v)]
    (if (not= v new-v)
      (zip/replace loc new-v)
      loc)))

(defn- with-move
  [matcher dirs]
  (fn [mr]
    (update mr :loc (fn [l] (-> (reduce #((moves %2) %1) l dirs)
                                (zip-edit matcher))))))

(defn- element->fun
  "returns a function which takes a loc and dirs, returns a function which takes a mr returns a new mr"
  [[loc dirs]]
  (fn [mr]
    (let [val (zip/node loc)
          matcher (cond
                    (sequential? val) (with-move identity dirs)
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
      (reduce #(%2 %1) mr fs))))

^:rct/test
(comment
  (run-query (compile-pattern 5) 5) ;=> 5
  (run-query (compile-pattern 5) 3) ;=> nil 
  (run-query (compile-pattern '[1 2]) [1 2]) ;=> [1 2]
  )