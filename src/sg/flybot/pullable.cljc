(ns sg.flybot.pullable 
  (:require 
   [clojure.zip :as zip] 
   [sg.flybot.pullable.core2 :as core]
   [sg.flybot.pullable.util :refer [cond-let]]))

(defn- element->fun
  "returns a function which takes a loc and dirs, returns a function which takes a mr returns a new mr"
  [[loc dirs]]
  (fn [mr]
    (let [val (zip/node loc)
          matcher (cond-let
                   [_ (list? val)]
                   (let [[sym modify?] (core/lvar (first val))]
                     (core/list-matcher (when-not (= sym '_) sym) modify? (rest val)))
                   
                   [_ (map? val)] (core/map-matcher val)

                   [_ (or (vector? val) (= val '_))] core/move

                   [_ (= val '$)] (core/terminal-matcher)

                   [[sym modify?] (core/lvar val)]
                   (core/pred-matcher (constantly true) sym modify?)

                   (core/literal val))]
      (matcher mr dirs))))

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
                (rest)
                (map element->fun))]
    (fn [mr]
      (reduce #(or (%2 %1) (reduced nil)) mr fs))))

(defn ptn-fn
  "returns a function from `pattern`"
  [pattern]
  (let [f (ptn-fn* pattern)]
    (fn [data]
      (-> data (core/data->mr) (f) (core/mr->data)))))

^:rct/test
(comment
  ((ptn-fn '{:a ?a}) {:a 5 :b 6})
  )
