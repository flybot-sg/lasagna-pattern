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
   ;; Sandbox mode
   :sandbox-data nil       ; Snapshot: {:users [...] :posts [...] :config {...}}
   :sandbox-schema nil     ; Malli hiccup form for autocomplete/tooltips
   :data-view :data        ; :data | :schema toggle
   ;; Remote mode
   :remote-data nil        ; Server snapshot: {:users [...] :posts [...] :config {...}}
   :server-url "http://localhost:8081/api"
   :schema nil             ; Remote server schema
   :schema-loading? false
   :schema-error nil
   :sample-data nil        ; Generated sample data from schema
   :schema-view-mode :schema ; :schema | :sample toggle
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
      (assoc :schema nil :schema-error nil :sample-data nil)
      (assoc :schema-view-mode :schema)))

(defn set-result [db result]
  (assoc db :loading? false :result result :error nil))

(defn set-error [db error]
  (assoc db :loading? false :error error :result nil))

(defn set-sandbox-data [db snapshot]
  (assoc db :sandbox-data snapshot))

(defn set-sandbox-schema [db schema]
  (assoc db :sandbox-schema schema))

(defn set-remote-data [db data]
  (assoc db :remote-data data))

;;=============================================================================
;; Remote mode updaters
;;=============================================================================

(defn set-schema-loading [db]
  (assoc db :schema nil :schema-error nil :schema-loading? true))

(defn set-schema [db schema]
  (assoc db :schema-loading? false :schema schema :schema-error nil :sample-data nil))

(defn set-schema-error [db error]
  (assoc db :schema-loading? false :schema nil :schema-error error :sample-data nil))

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
  ;; set-mode changes mode and clears results
  (let [db (set-mode {:mode :sandbox :result {:data 1}} :remote)]
    [(:mode db) (:result db)])
  ;=> [:remote nil]

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

  ;; set-sandbox-data stores snapshot
  (let [db (set-sandbox-data {} {:users [{:id 1}]})]
    (:sandbox-data db))
  ;=> {:users [{:id 1}]}

  ;; set-remote-data stores remote server snapshot
  (let [db (set-remote-data {} {:users [{:id 1}]})]
    (:remote-data db))
  ;=> {:users [{:id 1}]}

  ;; set-schema stores schema and clears sample-data
  (let [db (set-schema {:schema-loading? true :sample-data {:old "data"}} [:map [:name :string]])]
    [(:schema-loading? db) (:schema db) (:sample-data db)])
  ;=> [false [:map [:name :string]] nil]

  ;; move-autocomplete-selection wraps around
  (let [db {:autocomplete {:completions [:a :b :c] :selected 2}}]
    (:selected (:autocomplete (move-autocomplete-selection db 1)))))
  ;=> 0)
