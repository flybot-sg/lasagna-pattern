(ns sg.flybot.playground.ui.core.pull
  "Pull spec definitions — pattern + :then as data.

   Each named operation resolves to {:pattern ... :then ...}.
   :pattern is a pull pattern (data, not string).
   :then is (fn [response] effect-map).

   Pure data — no browser deps, testable on JVM."
  (:require [sg.flybot.playground.ui.core.db :as db]
            [sg.flybot.playground.common.data :as data]
            [sg.flybot.pullable.remote :as remote]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as reader])))

;;=============================================================================
;; Helpers
;;=============================================================================

(defn- read-pattern [s]
  #?(:clj (edn/read-string s)
     :cljs (reader/read-string s)))

(def ^:private read-all-pattern
  "Pull pattern that reads all top-level collections."
  (into {} (map (fn [k] [k (symbol (str "?" (name k)))])) (keys data/default-data)))

(defn- vars->data
  "Convert pull bindings (symbol keys from ?-vars) to keyword-keyed data map."
  [result]
  (into {} (map (fn [[k v]] [(keyword (name k)) v])) result))

(defn- mutation?
  "Returns true if pattern contains a mutation (nil key or nil value)."
  [pattern]
  (try
    (some? (remote/parse-mutation pattern))
    (catch #?(:clj Exception :cljs :default) _
      false)))

;;=============================================================================
;; Resolve
;;=============================================================================

(defn- role-map->schema
  "Wrap a role-keyed schema map into a single Malli hiccup form.
   {:guest [:map ...] :member [:maybe ...]} → [:map [:guest [:map ...]] [:member [:maybe ...]]]"
  [m]
  (when (map? m)
    (into [:map] (map (fn [[k v]] [k v])) m)))

^:rct/test
(comment
  ;; wraps role-keyed map into [:map ...] form
  (let [result (role-map->schema {:guest [:map [:posts :any]]
                                  :member [:maybe [:map [:me :any]]]})]
    [(first result) (set (rest result))])
  ;=> [:map #{[:guest [:map [:posts :any]]] [:member [:maybe [:map [:me :any]]]]}]

  (role-map->schema nil)
  ;=> nil
  nil)

(defn resolve-pull
  "Resolve a named pull operation to {:pattern ... :then ...}.
   :pattern is data (already parsed). :then receives response, returns effect map.
   Returns {:error msg} for parse failures."
  [op db]
  (case op
    :init
    (case (:mode db)
      :remote
      {:fetch (str (:server-url db) "/_schema")
       :then (fn [r] {:db #(db/set-remote-init % (update r :schema role-map->schema))})}

      ;; :sandbox (default)
      {:pattern (assoc read-all-pattern :schema '?s)
       :then (fn [r]
               (let [schema-data (get r 's)]
                 {:db #(-> %
                           (db/set-data (vars->data (dissoc r 's)))
                           (db/set-schema (:schema schema-data)))}))})

    :pattern
    (let [text (:pattern-text db)]
      (if (or (nil? text) (re-matches #"\s*" text))
        {:error "Enter a pattern to execute"}
        (try
          (let [pattern (read-pattern text)
                mut? (mutation? pattern)]
            {:pattern pattern
             :then (if mut?
                     (fn [result]
                       {:db #(-> %
                                 (db/set-result result)
                                 (db/apply-mutation-result pattern result))})
                     (fn [result]
                       {:db #(db/set-result % result)}))})
          (catch #?(:clj Exception :cljs :default) e
            {:error (str "Parse error: " #?(:clj (.getMessage e) :cljs (.-message e)))}))))

    :data
    {:pattern read-all-pattern
     :then (fn [r] {:db #(db/set-data % (vars->data r))})}

    :schema
    {:pattern '{:schema ?s}
     :then (fn [r]
             (let [schema-data (get r 's)]
               {:db #(db/set-schema % (:schema schema-data))}))}

    :seed
    {:pattern '{:seed {nil true}}
     :then (fn [_] {:db db/clear-result
                    :pull :data})}

    nil))

^:rct/test
(comment
  ;; :init sandbox — pattern includes data collections and schema
  (let [p (:pattern (resolve-pull :init {:mode :sandbox}))]
    [(contains? p :users) (contains? p :schema)])
  ;=> [true true]

  ;; :init remote — returns :fetch spec pointing to _schema endpoint
  (let [spec (resolve-pull :init {:mode :remote :server-url "http://localhost:8081/api"})]
    [(:fetch spec) (fn? (:then spec))])
  ;=> ["http://localhost:8081/api/_schema" true]

  ;; :pattern resolves user's text into data
  (:pattern (resolve-pull :pattern {:pattern-text "{:config ?cfg}"}))
  ;=> {:config ?cfg}

  ;; :pattern mutation — :then applies result to data snapshot
  (let [spec (resolve-pull :pattern {:pattern-text "{:users {nil {:name \"Dave\"}}}"})
        db {:data {:users [{:id 1 :name "Alice"}]}}
        effect ((:then spec) {:users {:id 4 :name "Dave"}})]
    (get-in ((:db effect) db) [:data :users 1 :name]))
  ;=> "Dave"

  ;; :pattern empty text returns error
  (:error (resolve-pull :pattern {:pattern-text ""}))
  ;=> "Enter a pattern to execute"

  (:error (resolve-pull :pattern {:pattern-text "   "}))
  ;=> "Enter a pattern to execute"

  ;; :pattern parse error returns {:error ...}
  (string? (:error (resolve-pull :pattern {:pattern-text "{invalid"})))
  ;=> true

  ;; :seed chains to :data fetch
  (:pull ((:then (resolve-pull :seed {})) nil))
  ;=> :data
  nil)
