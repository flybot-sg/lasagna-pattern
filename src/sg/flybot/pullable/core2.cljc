(ns sg.flybot.pullable.core2
  "Pull pattern for Clojure/script."
  (:require
   [clojure.zip :as zip]
   [sg.flybot.pullable.util :refer [either cond-let]])
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
   [_ (= :end (loc 1))] nil

   [l (and (zip/branch? loc) (zip/down loc))] [l [:down]]
   
   [l (zip/right loc)] [(zip/right loc) [:right]]

   (loop [p loc d []]
     (if-let [up (zip/up p)]
       (if-let [right-up (zip/right up)]
         [right-up (-> d (conj :up) (conj :right))]
         (recur up (conj d :up)))
       [[(zip/node p) :end] d]))))

(defn loc-dir-seq
  "use zip-next to generate a sequence of locs and directions"
  [loc]
  (->> (iterate (fn [[l _]] (let [[new-l ds] (zip-next l)] [new-l ds])) [loc []])
       (take-while (comp some? first))))

(defn elem-dir
  "returns a sequence of elements and directions from a `seq-loc-dir`"
  [seq-loc-dir]
  (map (fn [[l d]] [(zip/node l) d]) seq-loc-dir))

^:rct/test
(comment
  (-> (zip/vector-zip [1 2 [3 4] 5]) (loc-dir-seq) elem-dir) ;=> 
  [[[1 2 [3 4] 5] []]
   [1 [:down]] [2 [:right]] 
   [[3 4] [:right]] [3 [:down]] [4 [:right]]
   [5 [:up :right]]
   [[1 2 [3 4] 5] [:up]]]
  )

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

(defn pattern-zip
  "returns a zipper for patterns"
  [data]
  (zip/zipper
   (either vector? set? map?)
   identity
   (fn [node children]
     (with-meta (into (empty node) children) (meta node)))
   data))

^:rct/test
(comment
  (-> (comm-zip [1 2 (array-map :a 3 :b 4) '(5 6) (sorted-set 7 8)])
      (loc-dir-seq)
      (elem-dir)) ;=>
  [[[1 2 {:a 3, :b 4} [5 6] #{7 8}] []] [1 [:down]] [2 [:right]] [{:a 3, :b 4} [:right]] [[:a 3] [:down]] [:a [:down]] [3 [:right]] [[:b 4] [:up :right]]
   [:b [:down]] [4 [:right]] [[5 6] [:up :up :right]] [5 [:down]] [6 [:right]]
   [#{7 8} [:up :right]] [7 [:down]] [8 [:right]]
   [[1 2 {:a 3, :b 4} [5 6] #{7 8}] [:up :up]]]
  ;pattern zip
  (-> (pattern-zip [1 2 '(?a :when odd?)])
      (loc-dir-seq)
      (elem-dir)) ;=> ([[1 2 (?a :when odd?)] []] [1 [:down]] [2 [:right]] [(?a :when odd?) [:right]] [[1 2 (?a :when odd?)] [:up]])
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

;;----------------

(defn move
  [mr dirs]
  (let [moves {:down zip/down :right zip/right :up zip/up}
        mv (fn [l dir]
             (when-not (and (= :up dir) (zip/right l)) ;make sure there is no remain data 
               ((moves dir) l)))]
    (update mr :loc #(reduce mv % dirs))))

(defn map-matcher
  [m]
  (fn [mr]
    (tap> (str "loc" (:loc mr)))
    (update mr :loc zip/edit #(vary-meta (select-keys % (keys m)) assoc ::orig-map %))))

(defn lvar
  "returns lvar symbol if v is a lvar"
  [v]
  (when (symbol? v)
    (when-let [[_ n] (re-matches #"\?(\w+)" (name v))]
      (symbol n))))

^:rct/test
(comment
  (lvar '?a2s) ;=> a2s
  (lvar '6) ;=> nil
  )
