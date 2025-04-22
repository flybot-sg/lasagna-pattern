(ns hooks.cond-let
  (:require [clj-kondo.hooks-api :as api]))

(defn cond-let
  "Hook to analyze the cond-let macro, handling bindings introduced by if-let."
  [{:keys [node]}]
  {:node
   (api/list-node
    (list*
     (for [[binding body] (->> node :children rest (partition 2))]
       (api/list-node
        (list (api/token-node 'when-let) binding body)))))})
