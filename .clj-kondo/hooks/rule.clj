(ns hooks.rule
  (:require [clj-kondo.hooks-api :as api]))

(defn- parse-var-symbols
  "Parse a ?-prefixed symbol, returning all binding symbols to introduce.
   For quantified vars, returns both base and full form.
   Examples:
     ?x     -> [?x]
     ?name  -> [?name]
     ?x*    -> [?x ?x*]
     ?x+    -> [?x ?x+]
     ?x?    -> [?x ?x?]
     ?x*!   -> [?x ?x*!]
     ?_*    -> [] (wildcard, no binding)
     _      -> [] (wildcard)"
  [sym]
  (let [nm (name sym)]
    (when (and (= \? (first nm)) (> (count nm) 1))
      (if-let [[_ base] (re-matches #"\?(\S+?)([\?\+\*]\!?)" nm)]
        ;; Quantified variable - return both base and full form (skip wildcards)
        (when (not= "_" base)
          [(symbol (str "?" base)) sym])
        ;; Plain variable like ?x, ?name (skip ?_)
        (when (not= "?_" nm)
          [sym])))))

(defn- extract-vars
  "Extract ?-prefixed symbols from a pattern node, returning binding symbols"
  [node]
  (let [vars (atom #{})]
    (letfn [(walk [n]
              (cond
                (api/token-node? n)
                (let [v (api/sexpr n)]
                  (when (symbol? v)
                    (when-let [binding-syms (parse-var-symbols v)]
                      (swap! vars into binding-syms))))

                (api/vector-node? n)
                (run! walk (:children n))

                (api/map-node? n)
                (run! walk (:children n))

                (api/list-node? n)
                (run! walk (:children n))))]
      (walk node))
    @vars))

(defn rule
  "Hook for rule macro. Transforms:
   (rule pattern template)
   into:
   (fn [data] (let [?x data ?y data ...] template))

   This makes clj-kondo understand that ?x etc are bound."
  [{:keys [node]}]
  (let [[_ pattern template] (:children node)
        var-syms (extract-vars pattern)
        data-sym (api/token-node (gensym "data"))
        ;; Build let bindings: [?a data ?b data ...]
        bindings (into [] (mapcat (fn [v] [(api/token-node v) data-sym]) var-syms))
        let-node (api/list-node
                  [(api/token-node 'let)
                   (api/vector-node bindings)
                   template])
        fn-node (api/list-node
                 [(api/token-node 'fn)
                  (api/vector-node [data-sym])
                  let-node])]
    {:node fn-node}))
