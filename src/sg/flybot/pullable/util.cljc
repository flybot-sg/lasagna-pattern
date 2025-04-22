(ns sg.flybot.pullable.util)

(defn either
  "returns a predicate that accepts either `preds`"
  [& preds]
  (fn [x] (reduce #(or %1 (%2 x)) false preds)))

(defmacro cond-let
  {:clj-kondo/ignore [:unresolved-symbol :type-mismatch]}
  [& clauses]
  (when clauses
    (if (next clauses)
      (list 'if-let (first clauses)
            (second clauses)
            (cons 'cond-let (nnext clauses)))
      (first clauses))))

(defn take-until
  [pred coll]
  (let [[h [t]] (split-with pred coll)]
    (cond-> h t (concat [t]))))

^:rct/test
(comment
  (take-until odd? [1 1 3 4 5 6])) ;=> [1 1 3 4]
