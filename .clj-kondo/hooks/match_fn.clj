(ns hooks.match-fn
  (:require [clj-kondo.hooks-api :as api]))

(defn- parse-var-symbol
  "Parse a ?-prefixed symbol, returning the base binding symbol or nil.
   Examples:
     ?x     -> ?x
     ?name  -> ?name
     ?x*    -> ?x
     ?x+    -> ?x
     ?x?    -> ?x
     ?x*!   -> ?x
     ?_*    -> nil (wildcard, no binding)
     _      -> nil (wildcard)"
  [sym]
  (let [nm (name sym)]
    (when (and (= \? (first nm)) (> (count nm) 1))
      (if-let [[_ base] (re-matches #"\?(\S+?)[\?\+\*]\!?" nm)]
        ;; Quantified variable - extract base name (skip wildcards like ?_*)
        (when (not= "_" base)
          (symbol (str "?" base)))
        ;; Plain variable like ?x, ?name
        sym))))

(defn- extract-vars
  "Extract ?-prefixed symbols from a pattern node, returning binding symbols"
  [node]
  (let [vars (atom #{})]
    (letfn [(walk [n]
              (cond
                (api/token-node? n)
                (let [v (api/sexpr n)]
                  (when (symbol? v)
                    (when-let [binding-sym (parse-var-symbol v)]
                      (swap! vars conj binding-sym))))

                (api/vector-node? n)
                (run! walk (:children n))

                (api/map-node? n)
                (run! walk (:children n))

                (api/list-node? n)
                (run! walk (:children n))))]
      (walk node))
    @vars))

(defn- uses-symbol?
  "Check if a node tree contains a reference to the given symbol"
  [node sym]
  (let [found (atom false)]
    (letfn [(walk [n]
              (when-not @found
                (cond
                  (api/token-node? n)
                  (when (= sym (api/sexpr n))
                    (reset! found true))

                  (api/vector-node? n)
                  (run! walk (:children n))

                  (api/map-node? n)
                  (run! walk (:children n))

                  (api/list-node? n)
                  (run! walk (:children n)))))]
      (walk node))
    @found))

(defn match-fn
  "Hook for match-fn macro. Transforms:
   (match-fn pattern body)
   into:
   (fn [data] (let [$ data ?x data ...] body))
   Only binds $ if it's actually used in body."
  [{:keys [node]}]
  (let [[_ pattern body] (:children node)
        var-syms (extract-vars pattern)
        data-sym (api/token-node (gensym "data"))
        uses-$ (uses-symbol? body '$)
        ;; Build let bindings: [?a data ?b data ...] and optionally [$ data ...]
        bindings (cond-> []
                   uses-$ (into [(api/token-node '$) data-sym])
                   true (into (mapcat (fn [v] [(api/token-node v) data-sym]) var-syms)))
        let-node (api/list-node
                  [(api/token-node 'let)
                   (api/vector-node bindings)
                   body])
        fn-node (api/list-node
                 [(api/token-node 'fn)
                  (api/vector-node [data-sym])
                  let-node])]
    {:node fn-node}))
