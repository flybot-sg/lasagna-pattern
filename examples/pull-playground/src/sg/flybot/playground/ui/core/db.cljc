(ns sg.flybot.playground.ui.core.db
  "Application db — pure db→db updater functions.

   State lives under :app/playground in the app-db atom.
   Each updater takes a db map and returns a new db map.")

;;=============================================================================
;; DB Shape
;;=============================================================================

(def initial-state
  {:mode :sandbox          ; :sandbox | :remote (path-based: /sandbox, /remote)
   :pattern-text ""        ; Pattern editor content
   :result nil             ; Bindings map (symbol → value)
   :error nil              ; Error message string
   :loading? false
   :selected-example nil   ; Index of selected example
   :active-tab :pattern    ; Mobile tab: :pattern | :data | :examples
   ;; Sandbox store (set once at init, stable reference, nil in remote mode)
   :sandbox/store nil
   ;; Mode-agnostic (populated by handle-pull for current mode)
   :data nil               ; Snapshot: {:users [...] :posts [...] :config {...}}
   :schema nil             ; Malli hiccup form for autocomplete/tooltips
   :data-view :data        ; :data | :schema toggle
   ;; Remote mode config
   :server-url "https://www.flybot.sg/api"
   :schema-error nil
   :schema-loading? false
   ;; Autocomplete
   :autocomplete nil       ; {:completions [...] :selected 0 :prefix ":" :x :y}
   :version nil})          ; app version from meta tag

;;=============================================================================
;; Pure Updaters (db → db)
;;=============================================================================

(defn clear-result [db]
  (assoc db :result nil :error nil))

(defn set-loading [db]
  (assoc db :loading? true :error nil :result nil))

(defn set-mode [db mode]
  (-> db
      (assoc :mode mode)
      (assoc :result nil :error nil :selected-example nil)
      (assoc :data nil :schema nil :schema-error nil :schema-loading? false)))

(defn set-result [db result]
  (assoc db :loading? false :result result :error nil))

(defn set-error [db error]
  (assoc db :loading? false :error error :result nil))

(defn set-data [db data]
  (assoc db :data data))

(defn set-active-tab [db tab]
  (assoc db :active-tab tab))

;;=============================================================================
;; Remote mode updaters
;;=============================================================================

(defn set-schema [db schema]
  (assoc db :schema schema :schema-error nil))

(defn set-schema-error [db error]
  (assoc db :schema nil :schema-error error :schema-loading? false))

(defn set-remote-init [db {:keys [schema sample]}]
  (assoc db :schema schema :data sample :schema-error nil :schema-loading? false))

^:rct/test
(comment
  (let [db (set-remote-init {:schema nil :data nil :schema-error "old" :schema-loading? true}
                            {:schema [:map [:name :string]]
                             :sample {:users [{:name "Alice"}]}})]
    [(:schema db) (:data db) (:schema-error db) (:schema-loading? db)])
  ;=> [[:map [:name :string]] {:users [{:name "Alice"}]} nil false]
  nil)

;;=============================================================================
;; Mutation response → data snapshot
;;=============================================================================

(defn apply-mutation-result
  "Update data snapshot from a mutation response.
   Inspects the pattern to determine mutation type per collection key:
   - Create {nil data}   → append entity from response
   - Delete {{:id n} nil} → remove entity by query key
   - Update {{:id n} data} → replace entity with response
   Skips non-vector data (config, schema) gracefully."
  [db pattern result]
  (reduce-kv
   (fn [db coll-key coll-pattern]
     (let [items (get-in db [:data coll-key])]
       (if-not (and (map? coll-pattern) (vector? items))
         db
         (let [[query value] (first coll-pattern)
               entity (or (get result coll-key)
                          (get result (symbol (name coll-key))))]
           (cond
             ;; Create: {nil {...}} → append entity
             (and (nil? query) (some? value) (map? entity))
             (assoc-in db [:data coll-key] (conj items entity))

             ;; Delete: {{:id n} nil} → remove by query
             (and (map? query) (nil? value))
             (let [[id-k id-v] (first query)]
               (assoc-in db [:data coll-key]
                         (vec (remove #(= (get % id-k) id-v) items))))

             ;; Update: {{:id n} {...}} → replace with response entity
             (and (map? query) (map? value) (map? entity))
             (let [[id-k id-v] (first query)]
               (assoc-in db [:data coll-key]
                         (mapv #(if (= (get % id-k) id-v) entity %) items)))

             :else db)))))
   db
   pattern))

^:rct/test
(comment
  ;; Create: appends entity from response
  (let [db {:data {:users [{:id 1 :name "Alice"}]}}]
    (get-in (apply-mutation-result db
                                   {:users {nil {:name "Dave"}}}
                                   {:users {:id 4 :name "Dave"}})
            [:data :users]))
  ;=> [{:id 1 :name "Alice"} {:id 4 :name "Dave"}]

  ;; Delete: removes entity by query key
  (let [db {:data {:users [{:id 1} {:id 2}]}}]
    (get-in (apply-mutation-result db
                                   {:users {{:id 2} nil}}
                                   {:users true})
            [:data :users]))
  ;=> [{:id 1}]

  ;; Update: replaces with full entity from response
  (let [db {:data {:posts [{:id 1 :title "Old"}]}}]
    (get-in (apply-mutation-result db
                                   {:posts {{:id 1} {:title "New"}}}
                                   {:posts {:id 1 :title "New" :author "Alice"}})
            [:data :posts]))
  ;=> [{:id 1 :title "New" :author "Alice"}]

  ;; Result with symbol keys (remote/execute format)
  (let [db {:data {:users [{:id 1 :name "Alice"}]}}]
    (get-in (apply-mutation-result db
                                   {:users {nil {:name "Dave"}}}
                                   {'users {:id 4 :name "Dave"}})
            [:data :users]))
  ;=> [{:id 1 :name "Alice"} {:id 4 :name "Dave"}]

  ;; Non-vector data (config) is skipped
  (let [db {:data {:config {:v "1.0"}}}]
    (:data (apply-mutation-result db {:config {:v "2.0"}} {:config {:v "2.0"}})))
  ;=> {:config {:v "1.0"}}
  nil)

;;=============================================================================
;; Autocomplete updaters
;;=============================================================================

(defn show-autocomplete [db data]
  (assoc db :autocomplete data))

(defn hide-autocomplete [db]
  (assoc db :autocomplete nil))

(defn select-autocomplete [db idx]
  (assoc-in db [:autocomplete :selected] idx))

(defn move-autocomplete-selection [db direction]
  (let [{:keys [completions selected]} (:autocomplete db)
        n (count completions)]
    (assoc-in db [:autocomplete :selected] (mod (+ selected direction) n))))

^:rct/test
(comment
  ;; wraps around from last to first
  (let [db {:autocomplete {:completions [:a :b :c] :selected 2}}]
    (get-in (move-autocomplete-selection db 1) [:autocomplete :selected]))
  ;=> 0
  nil)
