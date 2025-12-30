(ns sg.flybot.pullable
  (:refer-clojure :exclude [compile])
  (:require [sg.flybot.pullable.core :as core]))

(def core-rules [core/named-var-rule core/vector-rule core/map-rule core/core-rule])
(defn compile 
  [ptn]
  (fn [data]
   (when-let [matcher (core/ptn->matcher ptn core-rules)]
     (matcher (core/vmr data)))))

(comment
  ((compile '{:a a :b {:c a}}) {:a 1 :b {:c 1}}))
