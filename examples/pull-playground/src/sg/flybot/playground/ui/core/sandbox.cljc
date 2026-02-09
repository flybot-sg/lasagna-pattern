(ns sg.flybot.playground.ui.core.sandbox
  "Sandbox mode — browser-side atom-source collections with CRUD.

   Detects mutations from pattern structure (nil key = create,
   nil value = delete) and executes patterns against atom-sources."
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as reader])
            [sg.flybot.playground.common.data :as data]
            [sg.flybot.pullable.collection :as coll]
            [sg.flybot.pullable.impl :as impl]
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
;; Sources
;;=============================================================================

(defonce sources (atom nil))

(defn- make-sources [data]
  {:users (coll/atom-source {:initial (:users data)})
   :posts (coll/atom-source {:initial (:posts data)})})

(defn- make-collections [srcs]
  {:users (coll/collection (:users srcs))
   :posts (coll/collection (:posts srcs))})

(defn snapshot []
  (when-let [srcs @sources]
    (let [colls (make-collections srcs)]
      {:users (vec (seq (:users colls)))
       :posts (vec (seq (:posts colls)))
       :config (:config data/default-data)})))

(defn init! []
  (reset! sources (make-sources data/default-data))
  (snapshot))

(defn reset-data! []
  (init!))

;;=============================================================================
;; Mutation Detection
;;=============================================================================

(defn- variable? [v]
  (and (symbol? v) (= \? (first (name v)))))

(defn parse-mutation
  "Detect if pattern is a mutation. Returns {:path :query :value} or nil.

   Mutation keys are nil (create) or map (update/delete).
   Keyword keys indicate nested read patterns."
  [pattern]
  (when (and (map? pattern) (= 1 (count pattern)))
    (let [[k v] (first pattern)]
      (when (and (keyword? k) (map? v) (= 1 (count v)))
        (let [[query value] (first v)]
          (when (and (not (variable? value))
                     (or (nil? query) (map? query)))
            {:path [k] :query query :value value}))))))

;;=============================================================================
;; Execution
;;=============================================================================

(defn- collection? [v]
  (instance? #?(:clj sg.flybot.pullable.collection.Collection
                :cljs coll/Collection) v))

(defn- realize
  "Convert Collection objects to plain vectors."
  [v]
  (cond
    (collection? v) (vec (seq v))
    (map? v) (into {} (map (fn [[k v]] [k (realize v)])) v)
    (vector? v) (mapv realize v)
    (seq? v) (map realize v)
    :else v))

(defn- read-pattern [s]
  #?(:clj (edn/read-string s)
     :cljs (reader/read-string s)))

(defn execute!
  "Execute a pattern string against sandbox collections.
   Auto-detects read vs mutation from pattern structure.
   Returns {:result bindings :snapshot data} or {:error message}."
  [pattern-str]
  (try
    (let [pattern (read-pattern pattern-str)]
      (if-let [{:keys [path query value]} (parse-mutation pattern)]
        (let [coll-key (first path)
              coll (get (make-collections @sources) coll-key)]
          (if (and coll (satisfies? coll/Mutable coll))
            {:result {coll-key (coll/mutate! coll query value)}
             :snapshot (snapshot)}
            {:error (str "Collection " coll-key " not found or not mutable")}))
        (let [colls (make-collections @sources)
              data (assoc colls :config (:config data/default-data))
              opts #?(:clj {}
                      :cljs {:resolve sci-resolve :eval sci-eval})
              matcher (impl/compile-pattern pattern opts)
              result (matcher (impl/vmr data))]
          (if (impl/failure? result)
            {:error (str "Match failed: " (:reason result)
                         (when (seq (:path result))
                           (str " at path " (:path result))))}
            {:result (realize (:vars result)) :snapshot (snapshot)}))))
    (catch #?(:clj Exception :cljs :default) e
      {:error (str "Error: " #?(:clj (.getMessage e) :cljs (.-message e)))})))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  (variable? '?x) ;=> true
  (variable? 'x) ;=> false
  (variable? :foo) ;=> false

  (parse-mutation '{:users {nil {:name "Dave"}}})
  ;=> {:path [:users] :query nil :value {:name "Dave"}}

  (parse-mutation '{:users {{:id 1} nil}})
  ;=> {:path [:users] :query {:id 1} :value nil}

  (parse-mutation '{:users {{:id 1} {:name "Updated"}}})
  ;=> {:path [:users] :query {:id 1} :value {:name "Updated"}}

  (parse-mutation '{:users ?all}) ;=> nil
  (parse-mutation '{:users {{:id 1} ?user}}) ;=> nil
  (parse-mutation '{:config {:debug (?d :default false)}}) ;=> nil

  (do (init!) (:result (execute! "{:config ?cfg}")))
  ;=>> {'cfg map?}

  (do (init!) (get-in (execute! "{:users ?all}") [:result 'all]))
  ;=>> vector?

  (do (init!)
      (-> (execute! "{:users {nil {:id 99 :name \"Dave\" :email \"d@e\" :role :user}}}")
          :snapshot :users count)))
  ;=> 4)
