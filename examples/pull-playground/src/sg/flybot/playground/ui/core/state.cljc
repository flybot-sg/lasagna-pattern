(ns sg.flybot.playground.ui.core.state
  "Application state — pure db→db updater functions.

   State lives under :app/playground in the app-db atom.
   Each updater takes a state map and returns a new state map.")

;;=============================================================================
;; State Shape
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
   :server-url "http://localhost:8081/api"
   :schema-error nil
   ;; Autocomplete
   :autocomplete nil})     ; {:completions [...] :selected 0 :prefix ":" :x :y}

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
      (assoc :data nil :schema nil :schema-error nil)))

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
  (assoc db :schema nil :schema-error error))

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

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; set-mode changes mode and clears results and data
  (let [db (set-mode {:mode :sandbox :result {:data 1} :data {:users []}} :remote)]
    [(:mode db) (:result db) (:data db)])
  ;=> [:remote nil nil]

  ;; clear-result nils out result and error
  (let [db (clear-result {:result {:a 1} :error "oops"})]
    [(:result db) (:error db)])
  ;=> [nil nil]

  ;; set-loading sets loading and clears result/error
  (let [db (set-loading {:loading? false :result {:a 1} :error "x"})]
    [(:loading? db) (:result db) (:error db)])
  ;=> [true nil nil]

  ;; set-result stores bindings
  (let [db (set-result {:loading? true} {'x 1})]
    [(:loading? db) (:result db)])
  ;=> [false {'x 1}]

  ;; set-error stores error
  (let [db (set-error {:loading? true} "Parse error")]
    [(:loading? db) (:error db)])
  ;=> [false "Parse error"]

  ;; set-data stores snapshot
  (let [db (set-data {} {:users [{:id 1}]})]
    (:data db))
  ;=> {:users [{:id 1}]}

  ;; set-schema stores schema and clears error
  (let [db (set-schema {:schema-error "old"} [:map [:name :string]])]
    [(:schema db) (:schema-error db)])
  ;=> [[:map [:name :string]] nil]

  ;; set-active-tab switches tab
  (let [db (set-active-tab {:active-tab :data} :pattern)]
    (:active-tab db))
  ;=> :pattern

  ;; move-autocomplete-selection wraps around
  (let [db {:autocomplete {:completions [:a :b :c] :selected 2}}]
    (:selected (:autocomplete (move-autocomplete-selection db 1)))))
  ;=> 0)
