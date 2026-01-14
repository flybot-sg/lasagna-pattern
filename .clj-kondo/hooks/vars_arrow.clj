(ns hooks.vars-arrow
  (:require [clj-kondo.hooks-api :as api]))

(defn vars->
  "Hook for vars-> macro. Transforms:
   (vars-> [sym1 sym2] body...)
   into:
   (fn [{:syms [sym1 sym2]}] body...)"
  [{:keys [node]}]
  (let [[_ bindings & body] (:children node)
        syms-key (api/keyword-node :syms)
        destructure-map (api/map-node [syms-key bindings])
        fn-node (api/token-node 'fn)]
    {:node (api/list-node (list* fn-node (api/vector-node [destructure-map]) body))}))
