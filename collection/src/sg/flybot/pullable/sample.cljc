(ns ^:no-doc sg.flybot.pullable.sample
  "Sample data generation from Malli schemas."
  (:require [malli.core :as m]
            [malli.util :as mu]
            [malli.generator :as mg]))

(defn- add-min-to-vectors
  "Walk schema and add :min to all :vector schemas."
  [schema min-size]
  (m/walk schema
          (m/schema-walker
           (fn [s]
             (if (= :vector (m/type s))
               (mu/update-properties s assoc :min min-size)
               s)))))

(defn generate
  "Generate sample data from a Malli schema.

   Options:
   - :size - Size hint for generated collections (default 10)
   - :seed - Random seed for reproducibility (default nil = random)
   - :min  - Minimum items in all vectors (default nil = no minimum)"
  ([schema] (generate schema {}))
  ([schema {:keys [size seed min] :or {size 10}}]
   (let [schema (cond-> (m/schema schema)
                  min (add-min-to-vectors min))]
     (mg/generate schema
                  (cond-> {:size size}
                    seed (assoc :seed seed))))))

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
