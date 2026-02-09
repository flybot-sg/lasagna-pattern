(ns sg.flybot.playground.ui.core
  "Playground SPA entry point.

   Uses a dispatch-of effect pattern:
   - :db    — pure state updater (swap! app-db update root-key f)
   - :exec  — pattern execution (sandbox or remote)
   - :batch — composed effects"
  (:require [replicant.dom :as r]
            [cognitect.transit :as t]
            [cljs.reader :as reader]
            [sg.flybot.playground.common.data :as data]
            [sg.flybot.playground.ui.core.state :as state]
            [sg.flybot.playground.ui.core.views :as views]
            [sg.flybot.playground.ui.core.sandbox :as sandbox]))

(defonce app-db (atom {:app/playground state/initial-state}))

(def ^:private root-key :app/playground)

;;=============================================================================
;; Remote API Client (Transit)
;;=============================================================================

(defn- transit-reader []
  (t/reader :json))

(defn- transit-writer []
  (t/writer :json))

(defn- encode [data]
  (t/write (transit-writer) data))

(defn- decode [s]
  (t/read (transit-reader) s))

(defn- format-error
  "Format an error response to a user-friendly string."
  [errors]
  (let [{:keys [code reason path]} (first errors)]
    (str (name code) ": " reason
         (when (seq path)
           (str " at " (pr-str path))))))

(defn- pull!
  "Execute a pull query against a remote server."
  [url pattern-str on-success on-error]
  (try
    (let [pattern (reader/read-string pattern-str)]
      (-> (js/fetch url
                    #js {:method "POST"
                         :headers #js {"Content-Type" "application/transit+json"
                                       "Accept" "application/transit+json"}
                         :body (encode {:pattern pattern})})
          (.then (fn [resp]
                   (if (.-ok resp)
                     (.text resp)
                     (throw (js/Error. (str "HTTP " (.-status resp)))))))
          (.then (fn [text]
                   (let [response (decode text)]
                     (if (:errors response)
                       (on-error (format-error (:errors response)))
                       (on-success response)))))
          (.catch (fn [err]
                    (on-error (.-message err))))))
    (catch :default e
      (on-error (str "Parse error: " (.-message e))))))

(defn- fetch-schema!
  "Fetch schema from remote server."
  [url on-success on-error]
  (let [schema-url (str url "/_schema")]
    (-> (js/fetch schema-url
                  #js {:method "GET"
                       :headers #js {"Accept" "application/transit+json"}})
        (.then (fn [resp]
                 (if (.-ok resp)
                   (.text resp)
                   (throw (js/Error. (str "HTTP " (.-status resp)))))))
        (.then (fn [text]
                 (on-success (decode text))))
        (.catch (fn [err]
                  (on-error (.-message err)))))))

;;=============================================================================
;; Exec Handlers
;;=============================================================================

(defn- exec-sandbox [{:keys [pattern]} dispatcher]
  (let [{:keys [result error snapshot]} (sandbox/execute! pattern)
        dispatch! (dispatcher root-key)]
    (if error
      (dispatch! {:db #(state/set-error % error)})
      (dispatch! {:db (fn [db]
                        (-> db
                            (state/set-result result)
                            (state/set-sandbox-data snapshot)))}))))

(defn- exec-sandbox-reset [_effect-def dispatcher]
  (let [snap (sandbox/reset-data!)
        dispatch! (dispatcher root-key)]
    (dispatch! {:db (fn [db]
                      (-> db
                          (state/set-sandbox-data snap)
                          state/clear-result))})))

(defn- exec-remote [{:keys [pattern url]} dispatcher]
  (let [dispatch! (dispatcher root-key)]
    (pull! url pattern
           (fn [result] (dispatch! {:db #(state/set-result % result)}))
           (fn [error] (dispatch! {:db #(state/set-error % error)})))))

(defn- fetch-schema-remote [{:keys [url]} dispatcher]
  (let [dispatch! (dispatcher root-key)]
    (fetch-schema! url
                   (fn [{:keys [schema sample]}]
                     (dispatch! {:db (fn [db]
                                       (-> db
                                           (state/set-schema schema)
                                           (assoc :sample-data sample)))}))
                   (fn [error]
                     (dispatch! {:db #(state/set-schema-error % error)})))))

(defn- exec-handler [effect-def dispatcher]
  (case (:type effect-def)
    :sandbox       (exec-sandbox effect-def dispatcher)
    :sandbox-reset (exec-sandbox-reset effect-def dispatcher)
    :remote        (exec-remote effect-def dispatcher)
    :fetch-schema  (fetch-schema-remote effect-def dispatcher)
    (js/console.warn "Unknown exec type:" (:type effect-def))))

;;=============================================================================
;; Dispatch
;;=============================================================================

(defn dispatch-of [app-db root-key]
  (fn [effects]
    (doseq [[type effect-def] effects]
      (case type
        :db    (swap! app-db update root-key effect-def)
        :exec  (exec-handler effect-def (partial dispatch-of app-db))
        :batch (doseq [[dispatch! eff] (effect-def @app-db (partial dispatch-of app-db))]
                 (dispatch! eff))
        (js/console.warn "Unknown effect type:" type)))))

;;=============================================================================
;; Rendering
;;=============================================================================

(defonce root-el (atom nil))

(defn render! []
  (when-let [el @root-el]
    (let [db (get @app-db root-key)
          dispatch! (dispatch-of app-db root-key)]
      (r/render el (views/app-view {::views/db db ::views/dispatch! dispatch!})))))

(add-watch app-db :render (fn [_ _ _ _] (render!)))

;;=============================================================================
;; Theme
;;=============================================================================

(defn ^:export toggle-theme! []
  (let [body (.-body js/document)
        new-theme (if (= (.getAttribute body "data-theme") "dark") "light" "dark")]
    (.setAttribute body "data-theme" new-theme)
    (js/localStorage.setItem "theme" new-theme)))

(defn- init-theme! []
  (when (= (js/localStorage.getItem "theme") "dark")
    (.setAttribute (.-body js/document) "data-theme" "dark")))

;;=============================================================================
;; Initialization
;;=============================================================================

(defn- detect-mode-from-url []
  (when (= (.get (js/URLSearchParams. (.-search js/location)) "mode") "remote")
    :remote))

(defn ^:export init! []
  (reset! root-el (js/document.getElementById "app"))
  (init-theme!)
  (let [snap (sandbox/init!)
        mode (detect-mode-from-url)
        dispatch! (dispatch-of app-db root-key)]
    (dispatch! {:db (fn [db]
                      (cond-> (-> db
                                  (state/set-sandbox-data snap)
                                  (state/set-sandbox-schema data/default-schema))
                        mode (state/set-mode mode)))}))
  (render!))

(init!)
