(ns sg.flybot.pullable.core2
  "Pull pattern for Clojure/script."
  (:require
   [clojure.zip :as zip]))

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
  "returns a predicate that accepts either `pred1` or `pred2`"
  [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))

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
  [[[1 2 [3 4] 5] []] [1 [:down]] [2 [:right]] [[3 4] [:right]] [3 [:down]] [4 [:right]] [5 [:up :right]]]
  )

(defn comm-zip
  "returns a common zipper support sequence, maps (as sequence of MapEntry)"
  [data]
  (zip/zipper 
   (either sequential? map?)
   seq
   (fn [node children]
     (with-meta children (meta node)))
   data))

^:rct/test
(comment
  (-> (comm-zip [1 2 (array-map :a 3 :b 4) '(5 6)])
      (loc-dir-seq)
      (elem-dir)
      vec) ;=>
  [[[1 2 {:a 3, :b 4} [5 6]] []] [1 [:down]] [2 [:right]] [{:a 3, :b 4} [:right]] [[:a 3] [:down]] [:a [:down]] [3 [:right]] [[:b 4] [:up :right]] [:b [:down]] [4 [:right]] [[5 6] [:up :up :right]] [5 [:down]] [6 [:right]]] 
  )
