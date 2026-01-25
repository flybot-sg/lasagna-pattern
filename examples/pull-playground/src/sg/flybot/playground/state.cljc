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
   :selected-example nil}) ; Index of selected example

;;=============================================================================
;; State Transitions (pure functions)
;;=============================================================================

(defn set-mode [state mode]
  {:state (-> state
              (assoc :mode mode)
              (assoc :result nil :error nil))})

(defn update-pattern [state text]
  {:state (assoc state :pattern-text text)})

(defn update-data [state text]
  {:state (assoc state :data-text text)})

(defn update-server-url [state url]
  {:state (assoc state :server-url url)})

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
    [(:loading? state) (:error state)]))
  ;=> [false "Parse error"])
