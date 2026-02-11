(ns sg.flybot.pullable.util
  "Pure utility functions and macros with no domain knowledge."
  #?(:cljs (:require-macros [sg.flybot.pullable.util])))

(defn variable?
  "Check if x is a ?-prefixed pattern variable symbol."
  [x]
  (and (symbol? x) (= \? (first (name x)))))

(defn contains-variables?
  "Check if x is or contains any ?-prefixed pattern variable symbol.
   Walks maps, vectors, lists, and sets."
  [x]
  (cond
    (variable? x) true
    (map? x)      (some (fn [[k v]] (or (contains-variables? k) (contains-variables? v))) x)
    (coll? x)     (some contains-variables? x)
    :else         false))

^:rct/test
(comment
  (variable? '?x) ;=> true
  (variable? '?_) ;=> true
  (variable? '?x*) ;=> true
  (variable? 'x) ;=> false
  (variable? :foo) ;=> false
  (variable? "?x") ;=> false

  ;; contains-variables? — bare variable
  (contains-variables? '?x) ;=> true

  ;; contains-variables? — map with variables
  (contains-variables? '{:title ?t}) ;=> true

  ;; contains-variables? — nested vector with variables
  (contains-variables? '{:tags [?first ?rest*]}) ;=> true

  ;; contains-variables? — extended variable form
  (contains-variables? '(?t :when string?)) ;=> true

  ;; contains-variables? — literal data (no variables)
  (contains-variables? '{:title "New"}) ;=> nil

  ;; contains-variables? — nil and scalars
  (contains-variables? nil) ;=> false
  (contains-variables? 42)) ;=> false)

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
