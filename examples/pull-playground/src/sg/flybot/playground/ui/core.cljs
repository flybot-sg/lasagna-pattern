(ns sg.flybot.playground.ui.core
  "Playground SPA entry point.

   Uses a dispatch-of effect pattern (like hibou):
   - :db    — pure state updater (swap! app-db update root-key f)
   - :pull  — pull pattern API (string or keyword, routed via pull-api)
   - :nav   — URL navigation (pushState)
   - :batch — composed effects"
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
  "Execute a pull query against a remote server.
   Always reads body — errors are transit-encoded in the response."
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

(defn- fetch-schema!
  "Fetch schema from remote server.
   Checks resp.ok — schema endpoint returns plain HTTP errors, not transit."
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
;; Pull API
;;=============================================================================

(def ^:private read-all-pattern
  (pr-str (into {} (map (fn [k] [k (symbol (str "?" (name k)))])) (keys data/default-data))))

(defn- pull-result->data
  "Convert pull bindings (transit symbol keys) to keyword-keyed data map."
  [result]
  (into {} (map (fn [[k v]] [(keyword (name k)) v])) result))

(def ^:private pull-api
  {:sandbox
   {:execute
    (fn [dispatch! _db pattern]
      (let [{:keys [result error snapshot]} (sandbox/execute! pattern)]
        (if error
          (dispatch! {:db #(state/set-error % error)})
          (dispatch! {:db #(-> % (state/set-result result)
                               (state/set-sandbox-data snapshot))}))))
    :reset
    (fn [dispatch! _db]
      (let [snap (sandbox/reset-data!)]
        (dispatch! {:db #(-> % (state/set-sandbox-data snap) state/clear-result)})))}

   :remote
   {:execute
    (fn [dispatch! db pattern]
      (let [url       (:server-url db)
            mutation? (try (some? (remote/parse-mutation (reader/read-string pattern)))
                           (catch :default _ false))]
        (pull! url pattern
               (fn [result]
                 (dispatch! {:db #(state/set-result % result)})
                 (when mutation?
                   (pull! url read-all-pattern
                          (fn [data]
                            (dispatch! {:db #(state/set-remote-data % (pull-result->data data))}))
                          (fn [_] nil))))
               (fn [error]
                 (dispatch! {:db #(state/set-error % error)})))))
    :init
    (fn [dispatch! db]
      (pull! (:server-url db) read-all-pattern
             (fn [result]
               (dispatch! {:db #(state/set-remote-data % (pull-result->data result))}))
             (fn [_] nil)))
    :schema
    (fn [dispatch! db]
      (fetch-schema! (:server-url db)
                     (fn [{:keys [schema sample]}]
                       (dispatch! {:db #(-> % (state/set-schema schema) (assoc :sample-data sample))}))
                     (fn [error]
                       (dispatch! {:db #(state/set-schema-error % error)}))))}})

;;=============================================================================
;; Dispatch
;;=============================================================================

(defn dispatch-of [app-db root-key]
  (fn [effects]
    (doseq [[type effect-def] effects]
      (case type
        :db    (swap! app-db update root-key effect-def)
        :pull  (let [db        (get @app-db root-key)
                     api       (get pull-api (:mode db))
                     dispatch! ((partial dispatch-of app-db) root-key)]
                 (if (string? effect-def)
                   ((:execute api) dispatch! db effect-def)
                   (when-let [f (get api effect-def)]
                     (f dispatch! db))))
        :nav   (let [path (str "/" (name effect-def))]
                 (when-not (= path (.-pathname js/location))
                   (.pushState js/history nil "" path)))
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

(defn- path->mode []
  (if (= (.-pathname js/location) "/remote")
    :remote
    :sandbox))

(defn ^:export init! []
  (reset! root-el (js/document.getElementById "app"))
  (init-theme!)
  (when (= (.-pathname js/location) "/")
    (.replaceState js/history nil "" "/sandbox"))
  (let [snap   (sandbox/init!)
        mode   (path->mode)
        dispatch! (dispatch-of app-db root-key)]
    (dispatch! {:db (fn [db]
                      (-> db
                          (state/set-sandbox-data snap)
                          (state/set-sandbox-schema data/default-schema)
                          (state/set-mode mode)))})
    (when (= mode :remote)
      (dispatch! {:pull :init}))
    (when-let [prev @popstate-listener]
      (.removeEventListener js/window "popstate" prev))
    (let [listener (fn [_] (dispatch! {:db #(state/set-mode % (path->mode))}))]
      (.addEventListener js/window "popstate" listener)
      (reset! popstate-listener listener)))
  (render!))

(init!)
