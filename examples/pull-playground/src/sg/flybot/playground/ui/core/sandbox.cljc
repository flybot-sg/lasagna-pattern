(ns sg.flybot.playground.ui.core.sandbox
  "Sandbox mode — browser-side atom-source collections with CRUD.

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
;; Sources & Collections (held, not ephemeral)
;;=============================================================================

(defonce ^:private sources* (atom nil))
(defonce ^:private store* (atom nil))
(defonce ^:private schema* (atom nil))

(defn- make-sources [raw-data]
  {:users (coll/atom-source {:initial (:users raw-data)})
   :posts (coll/atom-source {:initial (:posts raw-data)})})

(defn- make-store
  "Build the data map that api-fn returns.
   Includes collections, schema as pull-able data, and reset as a Mutable."
  [srcs]
  {:users   (coll/collection (:users srcs))
   :posts   (coll/collection (:posts srcs))
   :config  (:config data/default-data)
   :schema {:schema data/default-schema}
   :seed  (reify
            coll/Mutable
            (mutate! [_ _query _value]
              (let [new-srcs (make-sources data/default-data)]
                (reset! sources* new-srcs)
                (reset! store* (make-store new-srcs))
                true))
            coll/Wireable
            (->wire [_] nil))})

(defn- api-fn
  "Sandbox api-fn: same contract as server api-fn, backed by atom-sources."
  [_ctx]
  {:data   @store*
   :schema @schema*})

(defn init! []
  (let [srcs (make-sources data/default-data)]
    (reset! sources* srcs)
    (reset! store* (make-store srcs))
    (reset! schema* (m/schema (conj data/default-schema
                                    [:schema {:optional true} :any]
                                    [:seed {:optional true} :any])))))

;;=============================================================================
;; Execution
;;=============================================================================

(defn- read-pattern [s]
  #?(:clj (edn/read-string s)
     :cljs (reader/read-string s)))

(defn execute!
  "Execute a pattern string against sandbox collections.
   Delegates to remote/execute — same engine the server uses.
   Returns {:result vars} or {:error message}."
  [pattern-str]
  (try
    (let [pattern (read-pattern pattern-str)
          opts    #?(:clj  {}
                     :cljs {:resolve sci-resolve :eval-fn sci-eval})
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
  (do (init!) (:result (execute! "{:config ?cfg}")))
  ;=>> {'cfg map?}

  (do (init!) (get-in (execute! "{:users ?all}") [:result 'all]))
  ;=>> vector?

  ;; schema is pull-able
  (do (init!) (get-in (execute! "{:schema ?s}") [:result 's :schema]))
  ;=>> vector?

  ;; seed via mutation restores initial data
  (do (init!)
      (execute! "{:users {nil {:id 99 :name \"Dave\" :email \"d@e\" :role :user}}}")
      (execute! "{:seed {nil true}}")
      (-> (execute! "{:users ?all}") :result (get 'all) count)))
  ;=> 3)
