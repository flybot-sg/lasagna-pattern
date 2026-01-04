(ns sg.flybot.pullable
  "Public API for the pullable pattern matching library."
  (:refer-clojure :exclude [compile])
  (:require [sg.flybot.pullable.core :as core]))

(defn compile
  "Compile a pattern into a matcher function.

   The returned matcher takes a value and returns either:
   - A ValMatchResult with :val and :vars on success
   - A MatchFailure with diagnostic info on failure

   Example:
     (def m (compile '{:name ?name :age ?age}))
     (m {:name \"Alice\" :age 30})
     ;=> #ValMatchResult{:val {:name \"Alice\" :age 30} :vars {name \"Alice\" age 30}}"
  ([pattern]
   (compile pattern core/core-rules))
  ([pattern rules]
   (let [matcher (core/ptn->matcher pattern rules)]
     (fn [data]
       (matcher (core/vmr data))))))

^:rct/test
(comment
  (def m (compile '{:a ?a :b ?b}))
  (m {:a 1 :b 2}) ;=>>
  {:vars '{a 1 b 2}}
  (m {:a 1 :b 2 :c 3}) ;=>>
  {:vars '{a 1 b 2}}
  ;;failure returns MatchFailure with diagnostic info
  ((compile '{:x 10}) {:x 99}) ;=>>
  {:reason #"value mismatch" :path [:x]}
  ;;sequence pattern
  ((compile '[?a ?b]) [1 2]) ;=>>
  {:vars '{a 1 b 2}})
