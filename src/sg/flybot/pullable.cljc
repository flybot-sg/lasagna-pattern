(ns sg.flybot.pullable
  (:require 
   [clojure.zip :as zip]
   [sg.flybot.pullable.matcher :as matcher]
   [sg.flybot.pullable.core2 :as core]))

(defn- move
  [loc dirs]
  (let [moves {:down zip/down :right zip/right :up zip/up}]
    (reduce #((moves %2) %1) loc dirs)))

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
  )

(defn compile-pattern
  "returns a sequence of functions which takes a mr returns a new mr"
  [pattern]
  (let [fs (->> (core/pattern-zip pattern)
                (core/loc-dir-seq)
                (map element->fun))]
    (fn [mr]
      (reduce #(or (%2 %1) (reduced nil)) mr fs))))

(defn run-query
  [q data]
  (-> data core/data->mr q core/mr->data))

^:rct/test
(comment
  (run-query (compile-pattern '?a) 5) ;=> {a 5 & 5}
  (run-query (compile-pattern 5) 3) ;=> nil 
  (run-query (compile-pattern '[1 ?b]) [1 2]) ;=> {b 2 & [1 2]}
  (run-query (compile-pattern '[1 2]) [2 3]) ;=> nil
  (run-query (compile-pattern '{:a ?a}) {:a 1 :b 2}) ;=> {a 1 & {:a 1}}
  (run-query (compile-pattern '[1 (2 3) #{4} {5 6}]) '[1 (2 3) #{4} {5 6}]) ;=> {& [1 (2 3) #{4} {5 6}]}
  (run-query (compile-pattern '[1 {:b {:c ?c}}]) [1 {:a 1 :b {:c 3 :d 4}}]) ;=> {c 3 & [1 {:b {:c 3}}]} 
  (run-query (compile-pattern '[1 ?a {:a ?a}]) [1 2 {:a 2}]) ;=> {a 2 & [1 2 {:a 2}]}
  (run-query (compile-pattern [1 (list '?a :when even?)]) [1 2]) ;=> {a 2 & [1 2]}
  )