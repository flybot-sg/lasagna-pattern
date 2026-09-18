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
