(ns sg.flybot.playground.core
  "Playground SPA entry point - dispatch and effect execution."
  (:require [replicant.dom :as r]
            [sg.flybot.playground.state :as state]
            [sg.flybot.playground.views :as views]
            [sg.flybot.playground.local :as local]
            [sg.flybot.playground.api :as api]))

(defonce app-state (atom state/initial-state))

;;=============================================================================
;; Event Dispatch
;;=============================================================================

(declare dispatch!)

(def ^:private handlers
  "Maps event keywords to state functions."
  {:set-mode           (fn [s mode] (state/set-mode s mode))
   :update-pattern     (fn [s text] (state/update-pattern s text))
   :update-data        (fn [s text] (state/update-data s text))
   :update-server-url  (fn [s url] (state/update-server-url s url))
   :select-example     (fn [s example] (state/select-example s example))
   :set-selected-example (fn [s idx] {:state (assoc s :selected-example idx)})
   :execute            (fn [s _] (state/execute s))
   :execution-success  (fn [s result] (state/execution-success s result))
   :execution-error    (fn [s error] (state/execution-error s error))
   :clear-result       (fn [s _] (state/clear-result s))})

(defn- apply-handler [state event]
  (let [[event-type & args] (if (vector? event) event [event])
        handler (get handlers event-type)]
    (if handler
      (apply handler state args)
      (do (js/console.warn "Unknown event:" event-type)
          {:state state}))))

;;=============================================================================
;; Effect Execution
;;=============================================================================

(defn- execute-local! [{:keys [pattern data]}]
  (let [result (local/execute pattern data)]
    (if (:error result)
      (dispatch! [:execution-error (:error result)])
      (dispatch! [:execution-success result]))))

(defn- execute-remote! [{:keys [pattern url]}]
  (api/pull! url pattern
             (fn [result] (dispatch! [:execution-success result]))
             (fn [error] (dispatch! [:execution-error error]))))

(defn- execute-effects! [{:keys [local-exec remote-exec]}]
  (when local-exec (execute-local! local-exec))
  (when remote-exec (execute-remote! remote-exec)))

;;=============================================================================
;; Dispatch
;;=============================================================================

(defn dispatch!
  "Dispatch an event: :keyword or [:keyword arg1 arg2 ...]"
  [event]
  (let [old-state @app-state
        {:keys [state fx]} (apply-handler old-state event)]
    (reset! app-state state)
    (when fx (execute-effects! fx))))

;;=============================================================================
;; Rendering
;;=============================================================================

(defonce root-el (atom nil))

(defn render! []
  (when @root-el
    (r/render @root-el (views/app-view @app-state dispatch!))))

(add-watch app-state :render (fn [_ _ _ _] (render!)))

;;=============================================================================
;; Theme
;;=============================================================================

(defn ^:export toggle-theme! []
  (let [body (.-body js/document)
        current (.getAttribute body "data-theme")
        new-theme (if (= current "dark") "light" "dark")]
    (.setAttribute body "data-theme" new-theme)
    (js/localStorage.setItem "theme" new-theme)
    (render!)))

(defn- init-theme! []
  (let [saved (js/localStorage.getItem "theme")]
    (when (= saved "dark")
      (.setAttribute (.-body js/document) "data-theme" "dark"))))

;;=============================================================================
;; Initialization
;;=============================================================================

(defn ^:export init! []
  (js/console.log "Pull Pattern Playground initializing...")
  (reset! root-el (js/document.getElementById "app"))
  (init-theme!)
  (render!)
  (js/console.log "Pull Pattern Playground initialized"))

(init!)
