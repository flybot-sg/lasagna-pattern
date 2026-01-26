(ns sg.flybot.pullable.malli
  "Optional Malli schema integration. Require this namespace to enable.

   CLJ: Malli detected at runtime - works if malli is on classpath.
   CLJS: Requires malli in deps - add metosin/malli to your project.

   Usage:
     (require '[malli.core :as m])
     (require '[sg.flybot.pullable.malli])
     (compile-pattern '{:name ?n} {:schema (m/schema [:map [:name :string]])})"
  (:require [sg.flybot.pullable.schema :as schema]
            #?(:cljs [malli.core :as m])))

(def ^:private type-mapping
  {:string :string, :int :number, :double :number, :number :number
   :pos-int :number, :neg-int :number, :nat-int :number, :float :number
   :keyword :keyword, :qualified-keyword :keyword
   :symbol :symbol, :qualified-symbol :symbol
   :boolean :boolean, :true :boolean, :false :boolean
   :nil :any, :any :any, :some :any
   :map :map, :vector :seq, :sequential :seq, :set :seq
   :tuple :seq, :seqable :seq, :every :seq
   :enum :any, := :any, :maybe :any, :or :any, :and :any
   :not :any, :re :string, :fn :any, :ref :any, :schema :any, :=> :any, :-> :any})

(defn- make-malli-rule
  "Create a schema rule for Malli schemas."
  [m-schema? m-type m-children m-entries]
  (fn malli-rule [s]
    (when (m-schema? s)
      (let [t (m-type s)]
        (case t
          :map (let [em (into {} (map (fn [[k _ c]] [k c]) (m-entries s)))]
                 {:type :map
                  :child-schema #(get em %)
                  :valid-keys (when (seq em) (set (keys em)))})
          :tuple {:type :seq :child-schema #(nth (m-children s) % nil)}
          (:vector :sequential :set :seqable :every)
          (let [cs (m-children s)]
            {:type :seq :child-schema (when (seq cs) (constantly (first cs)))})
          :maybe (when-let [inner (first (m-children s))] (malli-rule inner))
          :or {:type (let [ts (->> (m-children s) (keep #(some-> (malli-rule %) :type)) set)]
                       (if (= 1 (count ts)) (first ts) :any))}
          :enum {:type (let [ts (set (map schema/infer-value-type (m-children s)))]
                         (if (= 1 (count ts)) (first ts) :any))}
          := {:type (schema/infer-value-type (first (m-children s)))}
          {:type (get type-mapping t :any)})))))

;; CLJ: runtime detection (optional dependency)
#?(:clj
   (let [malli-available? (try (require 'malli.core) true (catch Exception _ false))]
     (when malli-available?
       (let [m-schema? @(requiring-resolve 'malli.core/schema?)
             m-form @(requiring-resolve 'malli.core/form)
             schema-class (Class/forName "malli.core.Schema")]
         ;; Register schema rule for pattern matching
         (schema/register-schema-rule!
          (make-malli-rule
           m-schema?
           @(requiring-resolve 'malli.core/type)
           @(requiring-resolve 'malli.core/children)
           @(requiring-resolve 'malli.core/entries)))
         ;; Extend Wireable for serialization if collection is available
         (when (try (require 'sg.flybot.pullable.collection) true (catch Exception _ false))
           (let [wireable-protocol @(requiring-resolve 'sg.flybot.pullable.collection/Wireable)]
             (extend schema-class
               wireable-protocol
               {:->wire (fn [this] (m-form this))})))))))

;; CLJS: malli required at compile time via ns :require
;; Note: Wireable extension for CLJS should be done in the consuming project
;; that has both malli and collection as dependencies
#?(:cljs
   (schema/register-schema-rule!
    (make-malli-rule m/schema? m/type m/children m/entries)))
