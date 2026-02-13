(ns sg.flybot.playground.ui.core
  "Playground SPA entry point.

   Uses a dispatch-of effect pattern (same as flybot-site):
   - :db    — pure state updater (swap! app-db update root-key f)
   - :pull  — pull spec {:pattern ... :then ...} or keyword (resolved via pull/resolve-pull)
   - :nav   — URL navigation (pushState)

   dispatch-of creates a single stable closure with a volatile self-reference.
   Effects execute in effect-order — not map iteration order.
   Mode only affects transport: make-executor returns a function
   that runs patterns — sandbox does it in-process via remote/execute,
   remote sends HTTP. Everything else is mode-agnostic."
  (:require #?(:cljs [replicant.dom :as r])
            [cognitect.transit :as t]
            [sg.flybot.playground.common.data :as data]
            [sg.flybot.playground.ui.core.db :as db]
            [sg.flybot.playground.ui.core.pull :as pull]
            #?(:cljs [sg.flybot.playground.ui.core.views :as views])
            [sg.flybot.playground.ui.core.sandbox :as sandbox])
  #?(:clj (:import [java.io ByteArrayOutputStream ByteArrayInputStream])))

;;=============================================================================
;; Transit (cross-platform)
;;=============================================================================

(defn encode [data]
  #?(:clj  (let [out (ByteArrayOutputStream.)]
             (t/write (t/writer out :json) data)
             (.toString out "UTF-8"))
     :cljs (t/write (t/writer :json) data)))

(defn decode [s]
  #?(:clj  (t/read (t/reader (ByteArrayInputStream. (.getBytes ^String s "UTF-8")) :json))
     :cljs (t/read (t/reader :json) s)))

^:rct/test
(comment
  ;; Transit roundtrip — keywords
  (decode (encode {:a 1 :b "hello"}))
  ;=> {:a 1 :b "hello"}

  ;; Transit roundtrip — symbols (used in pull patterns)
  (decode (encode '{:users ?all}))
  ;=> {:users ?all}
  nil)

;;=============================================================================
;; Error Formatting
;;=============================================================================

(defn- format-error
  "Format an error response to a user-friendly string."
  [errors]
  (let [{:keys [code reason path]} (first errors)]
    (str (name code) ": " reason
         (when (seq path)
           (str " at " (pr-str path))))))

^:rct/test
(comment
  ;; formats code + reason
  (format-error [{:code :validation :reason "bad input"}])
  ;=> "validation: bad input"

  ;; includes path when present
  (format-error [{:code :schema :reason "invalid" :path [:users :name]}])
  ;=> "schema: invalid at [:users :name]"
  nil)

;;=============================================================================
;; App State
;;=============================================================================

(def ^:private root-key :app/playground)

#?(:cljs (defonce app-db (atom {root-key db/initial-state})))

;;=============================================================================
;; HTTP Transport (remote mode, browser only)
;;=============================================================================

#?(:cljs
   (defn- pull!
     "Execute a pull pattern against a remote server via HTTP POST.
      Takes pattern as data (not string).
      Callback-based: (on-success vars) or (on-error message)."
     [url pattern on-success on-error]
     (-> (js/fetch url
                   #js {:method "POST"
                        :headers #js {"Content-Type" "application/transit+json"
                                      "Accept" "application/transit+json"}
                        :body (encode {:pattern pattern})})
         (.then (fn [resp]
                  (-> (.text resp)
                      (.then (fn [text]
                               {:ok (.-ok resp)
                                :status (.-status resp)
                                :body (when (seq text) (decode text))})))))
         (.then (fn [{:keys [ok body status]}]
                  (if ok
                    (on-success body)
                    (let [error (if-let [err (first (:errors body))]
                                  (format-error [err])
                                  (str "HTTP " status))]
                      (on-error error)))))
         (.catch (fn [err]
                   (on-error (.-message err)))))))

;;=============================================================================
;; Executor — the ONLY mode-specific function
;;=============================================================================

#?(:cljs
   (defn- make-executor
     "Build executor for current mode.
      Returns (fn [pattern on-success on-error]).
      Sandbox: remote/execute in-process. Remote: HTTP POST."
     [db]
     (case (:mode db)
       :sandbox (fn [pattern on-success on-error]
                  (let [{:keys [result error]}
                        (sandbox/execute! (:sandbox/store db) sandbox/store-schema pattern)]
                    ;; Defer callback via microtask to avoid recursive dispatch
                    (if error
                      (js/queueMicrotask #(on-error error))
                      (js/queueMicrotask #(on-success result)))))
       :remote  (fn [pattern on-success on-error]
                  (pull! (:server-url db) pattern on-success on-error)))))

;;=============================================================================
;; Dispatch
;;=============================================================================

(def ^:private effect-order
  "Execution order for effect types.
   :db   — state update (pull handler sees updated state)
   :pull — pattern operation (reads current db snapshot)
   :nav  — pushState (last — URL reflects final state)"
  [:db :pull :nav])

#?(:cljs
   (defn dispatch-of
     "Create a stable dispatch function that routes effect maps.
      Returns a single closure with a self-reference via volatile —
      no closure recreation on pull callbacks.
      Effects execute in `effect-order` — not map iteration order."
     [app-db root-key]
     (let [self (volatile! nil)
           dispatch!
           (fn [effects]
             (doseq [type effect-order
                     :let [effect-def (get effects type)]
                     :when (some? effect-def)]
               (case type
                 :db   (swap! app-db update root-key effect-def)
                 :pull (let [db   (get @app-db root-key)
                             spec (if (and (map? effect-def) (:pattern effect-def))
                                    effect-def
                                    (pull/resolve-pull effect-def db))]
                         (cond
                           (:error spec)
                           (@self {:db #(db/set-error % (:error spec))})

                           (:pattern spec)
                           (let [{:keys [pattern then]} spec
                                 exec (make-executor db)]
                             (exec pattern
                                   (fn [r] (@self (if (fn? then) (then r) then)))
                                   (fn [e] (@self {:db #(db/set-error % e)}))))))
                 :nav  (let [path (str "/" (name effect-def))]
                         (when-not (= path (.-pathname js/location))
                           (.pushState js/history nil "" path))))))]
       (vreset! self dispatch!)
       dispatch!)))

#?(:cljs (def dispatch! (dispatch-of app-db root-key)))

;;=============================================================================
;; Theme
;;=============================================================================

#?(:cljs
   (defn ^:export toggle-theme! []
     (let [body (.-body js/document)
           new-theme (if (= (.getAttribute body "data-theme") "dark") "light" "dark")]
       (.setAttribute body "data-theme" new-theme)
       (js/localStorage.setItem "theme" new-theme))))

#?(:cljs
   (defn- init-theme! []
     (when (= (js/localStorage.getItem "theme") "dark")
       (.setAttribute (.-body js/document) "data-theme" "dark"))))

;;=============================================================================
;; Hot Reload
;;=============================================================================

#?(:cljs
   (defn ^:export render!
     "Trigger re-render after hot reload. Fires the add-watch on app-db."
     []
     (swap! app-db identity)))

;;=============================================================================
;; Initialization
;;=============================================================================

#?(:cljs
   (defn- path->mode []
     (if (= (.-pathname js/location) "/remote")
       :remote
       :sandbox)))

#?(:cljs
   (defn- on-popstate [_]
     (let [new-mode (path->mode)]
       (dispatch! {:db #(db/set-mode % new-mode)
                   :pull :init}))))

#?(:cljs
   (defn read-meta-version []
     (let [v (some-> (js/document.querySelector "meta[name='app-version']")
                     (.getAttribute "content"))]
       (when (and v (not= v "{{version}}"))
         v))))

#?(:cljs
   (defn ^:export init! []
     (let [store (sandbox/make-store (sandbox/make-sources data/default-data))]
       (init-theme!)
       (when-let [v (read-meta-version)]
         (swap! app-db update root-key assoc :version v))
       (when (= (.-pathname js/location) "/")
         (.replaceState js/history nil "" "/sandbox"))
       (add-watch app-db :render
                  (fn [_ _ _ state]
                    (when-let [el (js/document.getElementById "app")]
                      (r/render el (views/app-view {:sg.flybot.playground.ui.core.views/db (root-key state)
                                                    :sg.flybot.playground.ui.core.views/dispatch! dispatch!})))))
       (dispatch! {:db #(-> %
                            (assoc :sandbox/store store)
                            (db/set-mode (path->mode)))
                   :pull :init}))))

;; One-time popstate listener — defonce prevents duplicates on hot reload.
;; Uses the stable dispatch! reference (top-level def with volatile self-ref).
#?(:cljs (defonce _popstate (.addEventListener js/window "popstate" on-popstate)))

#?(:cljs (defonce _init (init!)))
