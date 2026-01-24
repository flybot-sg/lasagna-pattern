(ns sg.flybot.blog.ui.core
  "Blog SPA entry point - dispatch and effect execution.

   Views emit events, dispatch! routes to state functions and executes effects."
  (:require [replicant.dom :as r]
            [sg.flybot.blog.ui.state :as state]
            [sg.flybot.blog.ui.views :as views]
            [sg.flybot.blog.ui.api :as api]
            [sg.flybot.blog.ui.log :as log]
            [sg.flybot.blog.ui.history :as history]))

(defonce app-state (atom state/initial-state))

;;=============================================================================
;; Event Dispatch
;;=============================================================================

(declare dispatch!)

(def ^:private handlers
  "Maps event keywords to state functions.
   Events are either :keyword or [:keyword arg1 arg2 ...]"
  {:fetch-posts     (fn [s _] (state/fetch-posts s))
   :posts-fetched   state/posts-fetched
   :select-post     state/select-post
   :create-post     (fn [s _] (state/create-post s))
   :update-post     (fn [s _] (state/update-post s))
   :post-saved      state/post-saved
   :delete-post     state/delete-post
   :delete-confirmed state/delete-confirmed
   :post-deleted    state/post-deleted
   :view-new        (fn [s _] (state/view-new s))
   :view-edit       state/view-edit
   :view-back       state/set-view
   :update-form     state/update-form
   :view-history    (fn [s _] (state/view-history s))
   :history-fetched state/history-fetched
   :view-version    state/view-version
   :restore-version state/restore-version
   :restore-confirmed state/restore-confirmed
   :navigate        state/navigate
   :filter-by-tag   state/filter-by-tag
   :error           state/set-error
   ;; User/Auth
   :fetch-me        (fn [s _] (state/fetch-me s))
   :me-fetched      state/me-fetched
   :logout          (fn [s _] (state/logout s))})

(defn- apply-handler [state event]
  (let [[event-type & args] (if (vector? event) event [event])
        handler (get handlers event-type)]
    (if handler
      (apply handler state args)
      (do (log/warn "Unknown event:" event-type)
          {:state state}))))

;;=============================================================================
;; Effect Execution
;;=============================================================================

(defn- execute-api! [{:keys [pattern on-success on-error]}]
  (api/pull!
   pattern
   (fn [response]
     (let [result-key (some #(when (symbol? %) %) (keys response))
           result (get response result-key response)]
       (dispatch! [on-success result])))
   (fn [err]
     (dispatch! [on-error err]))))

(defn- execute-confirm! [{:keys [message on-confirm]}]
  (when (js/confirm message)
    (dispatch! on-confirm)))

(defn- execute-navigate! [url]
  (set! (.-location js/window) url))

(defn- execute-effects! [state {:keys [api confirm history navigate]}]
  (when api (execute-api! api))
  (when confirm (execute-confirm! confirm))
  (when (= history :push) (history/push-state! state))
  (when navigate (execute-navigate! navigate)))

;;=============================================================================
;; Dispatch
;;=============================================================================

(defn dispatch!
  "Dispatch an event: :keyword or [:keyword arg1 arg2 ...]"
  [event]
  (log/debug "Event:" event)
  (let [old-state @app-state
        {:keys [state fx]} (apply-handler old-state event)]
    (reset! app-state state)
    (when (not= (:view old-state) (:view state))
      (log/log-state-change (str (if (vector? event) (first event) event))
                            old-state state))
    (when fx (execute-effects! state fx))))

;;=============================================================================
;; Rendering
;;=============================================================================

(defonce root-el (atom nil))

(defn render! []
  (when @root-el
    (r/render @root-el (views/app-view @app-state dispatch!))))

(add-watch app-state :render (fn [_ _ _ _] (render!)))

;;=============================================================================
;; Initialization
;;=============================================================================

(defn- on-popstate [parsed]
  (dispatch! [:navigate parsed]))

(defn- init-from-url! []
  (when-let [parsed (history/path->state (history/current-path))]
    (dispatch! [:navigate parsed])
    (history/replace-state! @app-state)))

(defn ^:export init! []
  (log/info "Blog app initializing...")
  (reset! root-el (js/document.getElementById "app"))
  (history/init-history! on-popstate)
  (init-from-url!)
  (dispatch! :fetch-posts)
  (dispatch! :fetch-me)
  (render!)
  (log/info "Blog app initialized"))

(init!)
