(ns sg.flybot.playground.ui.core.sandbox
  "Sandbox mode — browser-side atom-source collections with CRUD.

   Stateless module: exports pure constructors and an execute! function
   that takes store + schema as args. No module-level atoms.
   Uses remote/execute directly: same execution engine as the server,
   but backed by atom-sources instead of a database.
   Schema and reset are pull-able data in the store, not special endpoints."
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as reader])
            [sg.flybot.playground.common.data :as data]
            [sg.flybot.pullable.collection :as coll]
            [sg.flybot.pullable.remote :as remote]
            [sg.flybot.pullable.malli]
            [malli.core :as m]
            #?(:cljs [sci.core :as sci])))

;;=============================================================================
;; SCI Context (ClojureScript only)
;;=============================================================================

#?(:cljs
   (def ^:private sci-ctx
     (sci/init {:namespaces {'clojure.core {'pos? pos?
                                            'neg? neg?
                                            'zero? zero?
                                            'even? even?
                                            'odd? odd?
                                            'string? string?
                                            'number? number?
                                            'keyword? keyword?
                                            'symbol? symbol?
                                            'map? map?
                                            'vector? vector?
                                            'seq? seq?
                                            'set? set?
                                            'nil? nil?
                                            'some? some?
                                            'empty? empty?
                                            'count count
                                            '= =
                                            'not= not=
                                            '< <
                                            '> >
                                            '<= <=
                                            '>= >=
                                            'identity identity}}})))

#?(:cljs
   (defn- sci-resolve [sym]
     (sci/eval-form sci-ctx sym)))

#?(:cljs
   (defn- sci-eval [form]
     (sci/eval-form sci-ctx form)))

;;=============================================================================
;; Sources & Store (stateless constructors)
;;=============================================================================

(defn make-sources
  "Create atom-sources for sandbox collections from raw data."
  [raw-data]
  {:users (coll/atom-source {:initial (:users raw-data)})
   :posts (coll/atom-source {:initial (:posts raw-data)})})

(defn make-store
  "Build the data map that api-fn returns.
   Includes collections, schema as pull-able data, and seed as a Mutable.
   Seed resets atom-sources to initial state (preserving original IDs)."
  [sources]
  {:users   (coll/collection (:users sources))
   :posts   (coll/collection (:posts sources))
   :config  (:config data/default-data)
   :schema {:schema data/default-schema}
   :seed  (reify
            coll/Mutable
            (mutate! [_ _query _value]
              (doseq [[k src] sources]
                (let [init-data (get data/default-data k)
                      init-map  (into {} (map (juxt :id identity)) init-data)
                      max-id    (apply max (keys init-map))]
                  (reset! (:db-atom src) init-map)
                  (reset! (:id-counter src) max-id)))
              true)
            coll/Wireable
            (->wire [_] nil))})

(def store-schema
  "Compiled Malli schema for sandbox store. Constant — computed once."
  (m/schema (conj data/default-schema
                  [:schema {:optional true} :any]
                  [:seed {:optional true} :any])))

;;=============================================================================
;; Execution
;;=============================================================================

(defn- read-pattern [s]
  #?(:clj (edn/read-string s)
     :cljs (reader/read-string s)))

(defn execute!
  "Execute a pattern string against sandbox collections.
   Delegates to remote/execute — same engine the server uses.
   Takes store and schema as args (no module-level state).
   Returns {:result vars} or {:error message}."
  [store schema pattern-str]
  (try
    (let [pattern (read-pattern pattern-str)
          opts    #?(:clj  {}
                     :cljs {:resolve sci-resolve :eval-fn sci-eval})
          api-fn  (fn [_ctx] {:data store :schema schema})
          result  (remote/execute api-fn pattern opts)]
      (if (:errors result)
        (let [{:keys [code reason]} (first (:errors result))]
          {:error (str (name code) ": " reason)})
        {:result result}))
    (catch #?(:clj Exception :cljs :default) e
      {:error (str "Error: " #?(:clj (.getMessage e) :cljs (.-message e)))})))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; execute! takes store + schema
  (let [srcs  (make-sources data/default-data)
        store (make-store srcs)]
    (:result (execute! store store-schema "{:config ?cfg}")))
  ;=>> {'cfg map?}

  (let [srcs  (make-sources data/default-data)
        store (make-store srcs)]
    (get-in (execute! store store-schema "{:users ?all}") [:result 'all]))
  ;=>> vector?

  ;; schema is pull-able
  (let [srcs  (make-sources data/default-data)
        store (make-store srcs)]
    (get-in (execute! store store-schema "{:schema ?s}") [:result 's :schema]))
  ;=>> vector?

  ;; seed via mutation restores initial data
  (let [srcs  (make-sources data/default-data)
        store (make-store srcs)]
    (execute! store store-schema "{:users {nil {:id 99 :name \"Dave\" :email \"d@e\" :role :user}}}")
    (execute! store store-schema "{:seed {nil true}}")
    (-> (execute! store store-schema "{:users ?all}") :result (get 'all) count))
  ;=> 3

  ;; seed preserves original IDs (no auto-increment bumping)
  (let [srcs  (make-sources data/default-data)
        store (make-store srcs)]
    (execute! store store-schema "{:users {nil {:id 99 :name \"Dave\" :email \"d@e\" :role :user}}}")
    (execute! store store-schema "{:seed {nil true}}")
    (->> (execute! store store-schema "{:users ?all}") :result (get 'all) (mapv :id) sort)))
  ;=> [1 2 3])
