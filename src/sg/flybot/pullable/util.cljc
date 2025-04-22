(ns ^:util-ns sg.flybot.pullable.util)

(defn either
  "returns a predicate that accepts either `preds`"
  [& preds]
  (fn [x] (reduce #(or %1 (%2 x)) false preds)))

(defmacro cond-let 
  [& clauses]
  (when clauses
    (if (next clauses)
      (list 'if-let (first clauses)
            (second clauses)
            (cons 'cond-let (nnext clauses)))
      (first clauses))))
