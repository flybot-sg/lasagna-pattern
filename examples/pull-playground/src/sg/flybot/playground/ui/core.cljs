(ns sg.flybot.playground.ui.core
  "Playground SPA entry point.

   Uses a dispatch-of effect pattern:
   - :db    — pure state updater (swap! app-db update root-key f)
   - :pull  — pull pattern API (string or keyword)
   - :nav   — URL navigation (pushState)

   Mode only affects transport. make-executor returns a function
   that runs patterns — sandbox does it in-process via remote/execute,
   remote sends HTTP. Everything else is mode-agnostic."
  (:require [replicant.dom :as r]
            [cognitect.transit :as t]
            [cljs.reader :as reader]
            [sg.flybot.playground.common.data :as data]
            [sg.flybot.playground.ui.core.state :as state]
            [sg.flybot.playground.ui.core.views :as views]
            [sg.flybot.playground.ui.core.sandbox :as sandbox]
            [sg.flybot.pullable.remote :as remote]))

(defonce app-db (atom {:app/playground state/initial-state}))

(def ^:private root-key :app/playground)

(defonce ^:private popstate-listener (atom nil))

;;=============================================================================
;; Transit Client (for remote mode HTTP transport)
;;=============================================================================

(defn- encode [data]
  (t/write (t/writer :json) data))

(defn- decode [s]
  (t/read (t/reader :json) s))

(defn- format-error
  "Format an error response to a user-friendly string."
  [errors]
  (let [{:keys [code reason path]} (first errors)]
    (str (name code) ": " reason
         (when (seq path)
           (str " at " (pr-str path))))))

(defn- pull!
  "Execute a pull pattern against a remote server via HTTP POST.
   Callback-based: (on-success vars) or (on-error message)."
  [url pattern-str on-success on-error]
  (try
    (let [pattern (reader/read-string pattern-str)]
      (-> (js/fetch url
                    #js {:method "POST"
                         :headers #js {"Content-Type" "application/transit+json"
                                       "Accept" "application/transit+json"}
                         :body (encode {:pattern pattern})})
          (.then (fn [resp] (.text resp)))
          (.then (fn [text]
                   (let [response (decode text)]
                     (if (:errors response)
                       (on-error (format-error (:errors response)))
                       (on-success response)))))
          (.catch (fn [err]
                    (on-error (.-message err))))))
    (catch :default e
      (on-error (str "Parse error: " (.-message e))))))

;;=============================================================================
;; Executor — the ONLY mode-specific function
;;=============================================================================

(def ^:private read-all-pattern
  "Pull pattern that reads all top-level collections."
  (pr-str (into {} (map (fn [k] [k (symbol (str "?" (name k)))])) (keys data/default-data))))

(defn- vars->data
  "Convert pull bindings (symbol keys from ?-vars) to keyword-keyed data map."
  [result]
  (into {} (map (fn [[k v]] [(keyword (name k)) v])) result))

(defn- make-executor
  "Build executor for current mode.
   Returns (fn [pattern-str on-success on-error]).
   Sandbox: remote/execute in-process. Remote: HTTP POST."
  [db]
  (case (:mode db)
    :sandbox (fn [pattern-str on-success on-error]
               (let [{:keys [result error]}
                     (sandbox/execute! (:sandbox/store db) sandbox/store-schema pattern-str)]
                 (if error (on-error error) (on-success result))))
    :remote  (fn [pattern-str on-success on-error]
               (pull! (:server-url db) pattern-str on-success on-error))))

;;=============================================================================
;; Pull API — mode-agnostic, everything is a pattern
;;=============================================================================

(defn- handle-pull
  "Execute a pull pattern operation against the data store and update UI state."
  [dispatch! db op]
  (let [exec (make-executor db)]
    (case op
      :pattern
      (let [pattern   (:pattern-text db)
            mutation? (try (some? (remote/parse-mutation (reader/read-string pattern)))
                           (catch :default _ false))]
        (exec pattern
              (fn [result]
                (dispatch! {:db #(state/set-result % result)})
                (when mutation?
                  (exec read-all-pattern
                        (fn [r] (dispatch! {:db #(state/set-data % (vars->data r))}))
                        (fn [_] nil))))
              (fn [error]
                (dispatch! {:db #(state/set-error % error)}))))

      :data
      (exec read-all-pattern
            (fn [r] (dispatch! {:db #(state/set-data % (vars->data r))}))
            (fn [_] nil))

      :schema
      (exec "{:schema ?s}"
            (fn [result]
              (let [{:keys [schema]} (get result 's)]
                (dispatch! {:db #(state/set-schema % schema)})))
            (fn [error]
              (dispatch! {:db #(state/set-schema-error % error)})))

      :seed
      (exec "{:seed {nil true}}"
            (fn [_]
              (exec read-all-pattern
                    (fn [r] (dispatch! {:db #(-> % (state/set-data (vars->data r)) state/clear-result)}))
                    (fn [_] nil)))
            (fn [error]
              (dispatch! {:db #(state/set-error % error)})))

      :init
      (do (handle-pull dispatch! db :data)
          (handle-pull dispatch! db :schema)))))

;;=============================================================================
;; Dispatch
;;=============================================================================

(defn dispatch-of [app-db root-key]
  (fn [effects]
    (doseq [[type effect-def] effects]
      (case type
        :db    (swap! app-db update root-key effect-def)
        :pull  (handle-pull (dispatch-of app-db root-key)
                            (get @app-db root-key)
                            effect-def)
        :nav   (let [path (str "/" (name effect-def))]
                 (when-not (= path (.-pathname js/location))
                   (.pushState js/history nil "" path)))
        (js/console.warn "Unknown effect type:" type)))))

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
;; Hot Reload
;;=============================================================================

(defn ^:export render!
  "Trigger re-render after hot reload. Fires the add-watch on app-db."
  []
  (swap! app-db identity))

;;=============================================================================
;; Initialization
;;=============================================================================

(defn- path->mode []
  (if (= (.-pathname js/location) "/remote")
    :remote
    :sandbox))

(defn ^:export init! []
  (let [el        (js/document.getElementById "app")
        dispatch! (dispatch-of app-db root-key)
        store     (sandbox/make-store (sandbox/make-sources data/default-data))]
    (init-theme!)
    (when (= (.-pathname js/location) "/")
      (.replaceState js/history nil "" "/sandbox"))
    (add-watch app-db :render
               (fn [_ _ _ state]
                 (r/render el (views/app-view {::views/db (root-key state) ::views/dispatch! dispatch!}))))
    (dispatch! {:db #(-> %
                         (assoc :sandbox/store store)
                         (state/set-mode (path->mode)))})
    (dispatch! {:pull :init})
    (when-let [prev @popstate-listener]
      (.removeEventListener js/window "popstate" prev))
    (let [listener (fn [_]
                     (let [new-mode (path->mode)]
                       (dispatch! {:db #(state/set-mode % new-mode)})
                       (dispatch! {:pull :init})))]
      (.addEventListener js/window "popstate" listener)
      (reset! popstate-listener listener))))

