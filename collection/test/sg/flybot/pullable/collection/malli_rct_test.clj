(ns sg.flybot.pullable.collection.malli-rct-test
  "RCT blocks that need Malli. Outside src so rct-clr skips them."
  (:require [sg.flybot.pullable.collection :as coll]
            [sg.flybot.pullable.collection.malli :refer [validated]]
            [sg.flybot.pullable.sample :refer [generate]]))

^:rct/test
(comment
  (def v-coll (coll/collection (coll/atom-source) {:indexes #{#{:id}}}))
  (def v (validated v-coll
                    {:query  [:map {:closed true} [:id :int]]
                     :create [:map {:closed true} [:name :string]]
                     :update [:map {:closed true} [:name {:optional true} :string]]}))

  (:name (coll/mutate! v nil {:name "Alice"})) ;=> "Alice"
  (:type (:error (coll/mutate! v nil {}))) ;=> :invalid-mutation
  (:type (:error (coll/mutate! v nil {:name "Bob" :admin? true})))
  ;=> :invalid-mutation

  ;; rejected input never reaches the inner collection
  (count v) ;=> 1

  (:message (:error (coll/mutate! v {:id 1} {:name 42})))
  ;=> "{:name [\"should be a string\"]}"
  (:name (coll/mutate! v {:id 1} {:name "Alice2"})) ;=> "Alice2"

  ;; the query is checked before the value
  (:message (:error (coll/mutate! v {:id "1"} {:name 42})))
  ;=> "{:id [\"should be an integer\"]}"

  ;; delete checks only the query
  (:type (:error (coll/mutate! v {:bogus 1} nil))) ;=> :invalid-mutation
  (coll/mutate! v {:id 1} nil) ;=> true
  (count v) ;=> 0

  (def v-update-only (validated v-coll {:update [:map {:closed true}]}))
  (:name (coll/mutate! v-update-only nil {:anything 1 :name "raw"}))
  ;=> "raw"
  (:type (:error (coll/mutate! v-update-only {:id 2} {:name "x"})))
  ;=> :invalid-mutation

  (:name (coll/mutate! (validated v-coll {}) nil {:name "anything"}))
  ;=> "anything"
  )

^:rct/test
(comment
  ;; Basic generation
  (int? (generate :int {:seed 42})) ;=> true

  ;; Map schema
  (map? (generate [:map [:id :int] [:name :string]] {:seed 42})) ;=> true

  ;; Vector with :min guarantees minimum items
  (let [result (generate [:vector :int] {:min 5 :seed 42})]
    (>= (count result) 5)) ;=> true
  )
