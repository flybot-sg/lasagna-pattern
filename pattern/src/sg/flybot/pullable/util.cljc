(ns sg.flybot.pullable.util
  "Pure utility functions and macros with no domain knowledge.")

(defmacro vars->
  "Create a fn that destructures vars map.
   (vars-> [sym child] (mvar sym child))
   expands to:
   (fn [vars] (let [sym (get vars 'sym) child (get vars 'child)] (mvar sym child)))"
  [bindings & body]
  (let [vars-sym (gensym "vars")]
    `(fn [~vars-sym]
       (let [~@(mapcat (fn [sym] [sym `(get ~vars-sym '~sym)]) bindings)]
         ~@body))))
