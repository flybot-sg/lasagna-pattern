(ns hooks.qfn
  (:require [clj-kondo.hooks-api :as api]))

(defn- extract-vars
  "Extract ?-prefixed symbols from a pattern node"
  [node]
  (let [vars (atom [])]
    (letfn [(walk [n]
              (cond
                (api/token-node? n)
                (let [v (api/sexpr n)]
                  (when (and (symbol? v)
                             (let [nm (name v)]
                               (and (= \? (first nm))
                                    (> (count nm) 1)
                                    (not (re-matches #"\?\S+[\?\+\*]\!?" nm)))))
                    (swap! vars conj n)))

                (api/vector-node? n)
                (run! walk (:children n))

                (api/map-node? n)
                (run! walk (:children n))

                (api/list-node? n)
                (run! walk (:children n))))]
      (walk node))
    @vars))

(defn qfn
  "Hook for qfn macro. Transforms:
   (qfn pattern body)
   into:
   (fn [data] (let [$ data ?x data ...] body))"
  [{:keys [node]}]
  (let [[_ pattern body] (:children node)
        var-nodes (extract-vars pattern)
        data-sym (api/token-node (gensym "data"))
        $-sym (api/token-node '$)
        ;; Build let bindings: [$ data ?a data ?b data ...]
        bindings (into [$-sym data-sym]
                       (mapcat (fn [v] [v data-sym]) var-nodes))
        let-node (api/list-node
                  [(api/token-node 'let)
                   (api/vector-node bindings)
                   body])
        fn-node (api/list-node
                 [(api/token-node 'fn)
                  (api/vector-node [data-sym])
                  let-node])]
    {:node fn-node}))
