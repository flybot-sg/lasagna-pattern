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
       (take-while #(not (zip/end? (first %))))))

(defn elem-dir
  "returns a sequence of elements and directions from a `seq-loc-dir`"
  [seq-loc-dir]
  (map (fn [[l d]] [(zip/node l) d]) seq-loc-dir))

(defn data-zip
  "returns a common data zipper support sequence, maps (as sequence of MapEntry)"
  [data]
  (zip/zipper
   coll?
   identity
   (fn [node children]
     (if (map-entry? node)
       (MapEntry. (first children) (second children)) ;has to patch for a map-entry
       (-> (into (empty node) children)
           (with-meta (meta node)))))
   data))

(defn pattern-zip
  "returns a zipper for patterns"
  [data]
  (zip/zipper
   (either vector? map?)
   identity
   (fn [node children]
     (with-meta (into (empty node) children) (meta node)))
   data))

^:rct/test
(comment
  ;data zip
  (-> (data-zip [1 2 (array-map :a 3 :b 4) '(5 6) (sorted-set 7 8)])
      (loc-dir-seq)
      (elem-dir)) ;=>
  [[[1 2 {:a 3, :b 4} [5 6] #{7 8}] []] [1 [:down]] [2 [:right]] [{:a 3, :b 4} [:right]] [[:a 3] [:down]] [:a [:down]] [3 [:right]] [[:b 4] [:up :right]]
   [:b [:down]] [4 [:right]] [[5 6] [:up :up :right]] [5 [:down]] [6 [:right]]
   [#{7 8} [:up :right]] [7 [:down]] [8 [:right]]]
  
  ;pattern zip
  (-> (pattern-zip [1 2 '(?a :when odd?)])
      (loc-dir-seq)
      (elem-dir)) ;=> ([[1 2 (?a :when odd?)] []] [1 [:down]] [2 [:right]] [(?a :when odd?) [:right]])
  )

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
  (-> nil data->mr mr->data) ;=> {& nil}
  )

;;----------------

(defn move
  "move `mr` in `dirs`, return a new mr"
  [mr dirs]
  (letfn [(moves [dir]
            (get {:down zip/down :right zip/right :up zip/up} dir
                 (constantly (ex-info "Unknown direction" {:dir dir}))))]
    (update mr :loc #(reduce (fn [l dir] ((moves dir) l)) % dirs))))

;;---------------------
;; ## Matcher factories
;;
;; A matcher is a function accept a `mr`, returns a new mr or nil if no match.

(defn map-matcher
  "A structure matcher factory for map data"
  [m]
  (fn [{:keys [loc] :as mr}]
    (assoc mr 
           :loc
           (let [[n p] loc]
             ;modified version of `zip/replace`
             (with-meta
               [(-> (select-keys n (keys m))
                    (with-meta (assoc (meta n) ::orig-map n)))
                (assoc p :changed? true)]
               (meta loc))))))

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

(defn pred-matcher
  "returns a matcher that accepts a predicate `pred` and a optional symbol `lvar`"
  ([pred] (pred-matcher pred nil))
  ([pred lvar]
   (pred-matcher pred lvar false))
  ([pred lvar modify?]
   (fn [mr]
     (let [v (some-> mr :loc zip/node)
           pre-v (some-> mr :vars (get lvar))
           pred (if pre-v (fn [v] (and (= pre-v v) (pred v))) pred)
           v' (pred v)]
       (if (not modify?)
         (when v' ;FIXME can not return nil
           (cond-> mr
             lvar (assoc-in [:vars lvar] v)))
         (cond-> mr
           lvar (assoc-in [:vars lvar] v')
           true (update :loc (fn [loc] (if (= :sg.flybot.pullable/remove v')
                                         (zip/remove loc)
                                         (zip/replace loc v'))))))))))

(defn literal
  [val]
  (pred-matcher #(= % val)))

^:rct/test
(comment
  ((pred-matcher even? 'a) {:loc [4 nil]}) ;=>  {:loc [4 nil], :vars {a 4}} 
  ((literal 4) {:loc [4 nil]}) ;=>  {:loc [4 nil]}
  ; pred-matcher with lvar bound
  ((pred-matcher even? 'a) {:loc [4 nil] :vars '{a 2}}) ;=>  nil
  )

(defmulti keyword->func 
  "returns a predict function. The argument is a sequence."
  first)

(defn list-matcher
  "returns a list matcher with symbol `sym` and optional `args`"
  [sym args]
  (let [[f modify?] [(keyword->func args) (-> args first name (.endsWith "!"))]]
    (pred-matcher f sym modify?)))

(comment
  ((list-matcher 'a (list :when even?)) {:loc (data-zip 4)})
  )

(defn terminal-matcher
  "A matcher that matches the end of a sequence"
  []
  (fn [mr]
    (when-not (some-> mr :loc zip/right)
      mr)))

;-------------------
; ## keyword dispatch

(defmethod keyword->func :when
  [[_ pred]]
  (fn [v] (pred v)))

(defmethod keyword->func :to!
  [[_ val]]
  (fn [_]  val))

(defmethod keyword->func :edit!
  [[_ f]]
  (fn [v] (f v)))

(defmethod keyword->func :with!
  [[_ val]]
  (fn [f]
    (if ((either fn? ifn?) f)
      (f val)
      (throw (ex-info "Expect a function" {:f f})))))