(ns sg.flybot.pullable 
  (:require 
   [clojure.zip :as zip]
   [sg.flybot.pullable.matcher :as matcher]
   [sg.flybot.pullable.core2 :as core]))

(defn- move
  [loc dirs]
  (let [moves {:down zip/down :right zip/right :up zip/up}]
    (reduce (fn [l dir] (when-not (and (= :up dir) (zip/right l)) ;make sure there is no remain data 
                          ((moves dir) l)))
            loc dirs)))

(defn- map-matcher
  [m]
  (fn [mr]
    (update mr :loc zip/edit #(select-keys % (keys m)))))

(defn- lvar
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

(defn- element->fun
  "returns a function which takes a loc and dirs, returns a function which takes a mr returns a new mr"
  [[loc dirs]]
  (fn [mr]
    (let [val (zip/node loc)
          matcher (cond
                    (and (list? val) (lvar (first val)))
                    (apply matcher/list-matcher (lvar (first val)) (rest val))

                    (sequential? val) identity
                    (map? val) (map-matcher val)

                    (lvar val)
                    (matcher/pred-matcher (constantly true) (lvar val)) 
                    
                    :else
                    (matcher/literal val))]
      (matcher (update mr :loc move dirs)))))

^:rct/test
(comment
  ((element->fun [[[1 2] nil] []]) {:loc [[1 2] nil]}) ;=> {:loc [[1 2] nil]}
  ((element->fun [['?a nil] []]) {:loc [3 nil]}) ;=> {:loc [3 nil] :vars {a 3}}
  ((element->fun [[2 {:l [1] :pnodes [[1 2]] :r []}] [:up]]) 
   {:loc [2 {:l [1] :pnodes [[1 2 3]] :r [3]}]}) ;=> nil
  )

(defn ptn-fn*
  "returns a sequence of functions which takes a mr returns a new mr"
  [pattern]
  (let [fs (->> (core/pattern-zip pattern)
                (core/loc-dir-seq)
                (map element->fun))]
    (fn [mr]
      (reduce #(or (%2 %1) (reduced nil)) mr fs))))

(defn ptn-fn
  "returns a function from `pattern`"
  [pattern]
  (let [f (ptn-fn* pattern)]
    (fn [data]
      (-> data (core/data->mr) (f) (core/mr->data)))))
