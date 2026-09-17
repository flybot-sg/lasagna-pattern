(ns ^:no-doc sg.flybot.pullable.collection.malli
  "Malli validation of mutation input. Requires metosin/malli."
  (:require [malli.core :as m]
            [malli.error :as me]
            [sg.flybot.pullable.collection :as coll]))

(defn- schema-error
  "nil when `value` conforms to `schema`, error map otherwise."
  [schema value]
  (when-let [explanation (some-> schema (m/explain value))]
    {:error {:type :invalid-mutation
             :message (pr-str (me/humanize explanation))}}))

(defn validated
  "Wrap `target` so mutation input is validated against Malli schemas:
   `:query` checks the query on update and delete, `:create` and `:update`
   check the value. Omitted keys are not checked. Invalid input returns
   {:error {:type :invalid-mutation :message <humanized>}} and never reaches
   `target`. Wrap outermost so it sees the raw client input."
  [target {:keys [query create update]}]
  (coll/wrap-mutable target
                     (fn [inner q value]
                       (or (when (some? q) (schema-error query q))
                           (when (some? value)
                             (schema-error (if (nil? q) create update) value))
                           (coll/mutate! inner q value)))))

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
