(ns sg.flybot.playground.state
  "Application state - pure functions returning {:state ... :fx ...}.

   Each function returns a map with:
   - :state - the new state
   - :fx    - optional effects map {:local-exec ... :remote-exec ...}")

;;=============================================================================
;; State Shape
;;=============================================================================

(def initial-state
  {:mode :local           ; :local | :remote
   :pattern-text ""       ; Pattern editor content
   :data-text ""          ; Data editor content (local mode)
   :server-url "http://localhost:8081/api"  ; Server URL (remote mode)
   :result nil            ; bindings map (symbol -> value) or nil
   :error nil             ; Error message string or nil
   :loading? false
   :selected-example nil  ; Index of selected example
   :schema nil            ; Remote server schema (remote mode only)
   :schema-loading? false
   :schema-error nil
   :sample-data nil       ; Generated sample data from schema (remote mode)
   :schema-view-mode :schema  ; :schema | :sample - toggle in schema viewer
   :sidebar-collapsed? false  ; Hide examples panel (remote mode)
   ;; Autocomplete state
   :autocomplete nil})    ; {:completions [...] :selected 0 :prefix ":" :x :y}

;;=============================================================================
;; State Transitions (pure functions)
;;=============================================================================

(defn set-mode [state mode]
  {:state (-> state
              (assoc :mode mode)
              (assoc :result nil :error nil)
              (assoc :schema nil :schema-error nil :sample-data nil)
              (assoc :schema-view-mode :schema))})

(defn update-pattern [state text]
  {:state (assoc state :pattern-text text)})

(defn update-data [state text]
  {:state (assoc state :data-text text)})

(defn update-server-url [state url]
  ;; Just update URL - debounced fetch happens via :fetch-schema event
  {:state (assoc state :server-url url)})

(defn fetch-schema [state]
  ;; Trigger schema fetch for current URL (also sets loading state)
  {:state (assoc state :schema nil :schema-error nil :schema-loading? true)
   :fx {:fetch-schema {:url (:server-url state)}}})

(defn select-example [state {:keys [pattern data]}]
  {:state (-> state
              (assoc :pattern-text pattern)
              (assoc :data-text data)
              (assoc :result nil :error nil))})

(defn execute [state]
  {:state (assoc state :loading? true :error nil :result nil)
   :fx (if (= :local (:mode state))
         {:local-exec {:pattern (:pattern-text state)
                       :data (:data-text state)}}
         {:remote-exec {:pattern (:pattern-text state)
                        :url (:server-url state)}})})

(defn execution-success [state result]
  {:state (assoc state :loading? false :result result :error nil)})

(defn execution-error [state error]
  {:state (assoc state :loading? false :error error :result nil)})

(defn clear-result [state]
  {:state (assoc state :result nil :error nil)})

(defn set-selected-example [state idx]
  {:state (assoc state :selected-example idx)})

(defn fetch-schema-success [state schema]
  {:state (assoc state :schema-loading? false :schema schema :schema-error nil :sample-data nil)})

(defn fetch-schema-error [state error]
  {:state (assoc state :schema-loading? false :schema nil :schema-error error :sample-data nil)})

(defn set-sample-data [state sample-data]
  {:state (assoc state :sample-data sample-data)})

(defn set-schema-view-mode [state mode]
  {:state (assoc state :schema-view-mode mode)})

(defn toggle-sidebar [state]
  {:state (update state :sidebar-collapsed? not)})

;;=============================================================================
;; Autocomplete
;;=============================================================================

(defn show-autocomplete [state autocomplete-data]
  {:state (assoc state :autocomplete autocomplete-data)})

(defn hide-autocomplete [state]
  {:state (assoc state :autocomplete nil)})

(defn select-autocomplete [state idx]
  {:state (assoc-in state [:autocomplete :selected] idx)})

(defn move-autocomplete-selection [state direction]
  (let [{:keys [completions selected]} (:autocomplete state)
        n (count completions)
        new-idx (mod (+ selected direction) n)]
    {:state (assoc-in state [:autocomplete :selected] new-idx)}))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; set-mode changes mode and clears results
  (let [{:keys [state]} (set-mode {:mode :local :result {:data 1}} :remote)]
    [(:mode state) (:result state)])
  ;=> [:remote nil]

  ;; update-pattern updates pattern text
  (let [{:keys [state]} (update-pattern initial-state "{:name ?n}")]
    (:pattern-text state))
  ;=> "{:name ?n}"

  ;; execute returns local-exec effect in local mode
  (let [{:keys [fx]} (execute {:mode :local :pattern-text "p" :data-text "d"})]
    (:local-exec fx))
  ;=> {:pattern "p" :data "d"}

  ;; execute returns remote-exec effect in remote mode
  (let [{:keys [fx]} (execute {:mode :remote :pattern-text "p" :server-url "http://x"})]
    (:remote-exec fx))
  ;=> {:pattern "p" :url "http://x"}

  ;; execution-success stores result (bindings map)
  (let [{:keys [state]} (execution-success {:loading? true} {'x 1})]
    [(:loading? state) (:result state)])
  ;=> [false {'x 1}]

  ;; execution-error stores error
  (let [{:keys [state]} (execution-error {:loading? true} "Parse error")]
    [(:loading? state) (:error state)])
  ;=> [false "Parse error"]

  ;; fetch-schema returns fetch-schema effect
  (let [{:keys [fx]} (fetch-schema {:server-url "http://test/api"})]
    (:fetch-schema fx))
  ;=> {:url "http://test/api"}

  ;; fetch-schema-success stores schema and clears sample-data
  (let [{:keys [state]} (fetch-schema-success {:schema-loading? true :sample-data {:old "data"}} {:users [:name :email]})]
    [(:schema-loading? state) (:schema state) (:sample-data state)])
  ;=> [false {:users [:name :email]} nil]

  ;; fetch-schema-error stores error and clears sample-data
  (let [{:keys [state]} (fetch-schema-error {:schema-loading? true :sample-data {:old "data"}} "Network error")]
    [(:schema-loading? state) (:schema-error state) (:sample-data state)])
  ;=> [false "Network error" nil]

  ;; set-sample-data stores generated data
  (let [{:keys [state]} (set-sample-data {} {:users [{:name "Alice"}]})]
    (:sample-data state))
  ;=> {:users [{:name "Alice"}]}

  ;; set-schema-view-mode changes view mode
  (let [{:keys [state]} (set-schema-view-mode {:schema-view-mode :schema} :sample)]
    (:schema-view-mode state))
  ;=> :sample

  ;; toggle-sidebar toggles collapsed state
  (let [{:keys [state]} (toggle-sidebar {:sidebar-collapsed? false})]
    (:sidebar-collapsed? state))
  ;=> true

  (let [{:keys [state]} (toggle-sidebar {:sidebar-collapsed? true})]
    (:sidebar-collapsed? state)))
  ;=> false)
