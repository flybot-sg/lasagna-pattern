(ns sg.flybot.pullable.matcher
  (:require
   [clojure.zip :as zip]))

;; A matcher is a function that takes a mr and returns a mr.
;; If returns nil means no match.

(defn pred-matcher
  [pred lvar]
  (fn [mr]
    (let [v (some-> mr :loc zip/node)]
      (when (pred v)
        (cond-> mr
          lvar (assoc-in [:vars lvar] v))))))

(defn literal
  [val]
  (pred-matcher #(= % val) nil))

^:rct/test
(comment
  ((pred-matcher even? 'a) {:loc [4 nil]}) ;=>  {:loc [4 nil], :vars {a 4}} 
  ((literal 4) {:loc [4 nil]}) ;=>  {:loc [4 nil]}
  )