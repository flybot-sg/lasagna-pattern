(ns sg.flybot.flybot-site.ui.core
  "Blog SPA entry point — dispatch-of effect pattern.

   dispatch-of creates a dispatch function that routes effect maps:
   - :db       — pure state updater (swap! app-db update root-key f)
   - :pull     — pull spec {:pattern ... :then ...} or keyword (resolved via pull/resolve-pull)
   - :confirm  — js/confirm with on-confirm effect map
   - :history  — pushState URL update
   - :navigate — hard navigation (set! location)
   - :toast    — auto-dismiss toast notification"
  (:require #?(:cljs [replicant.dom :as r])
            [cognitect.transit :as t]
            [sg.flybot.flybot-site.ui.core.db :as db]
            [sg.flybot.flybot-site.ui.core.pull :as pull]
            #?(:cljs [sg.flybot.flybot-site.ui.core.views :as views])
            [sg.flybot.flybot-site.ui.core.log :as log]
            [sg.flybot.flybot-site.ui.core.history :as history])
  #?(:clj (:import [java.io ByteArrayOutputStream ByteArrayInputStream])))

(def ^:private api-url "/api")

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
  ;; Transit encode/decode roundtrip — keywords
  (decode (encode {:a 1 :b "hello"}))
  ;=> {:a 1 :b "hello"}

  ;; Transit encode/decode roundtrip — nested maps
  (decode (encode {:posts [{:post/id 1 :post/title "Hi"}]}))
  ;=> {:posts [{:post/id 1 :post/title "Hi"}]}

  ;; Transit encode/decode roundtrip — symbols (used in pull patterns)
  (decode (encode '{:guest {:posts ?all}})))
  ;=> {:guest {:posts ?all}})

#?(:cljs
   (defn pull!
     "Execute a pull query against the API.

      pattern - Pull pattern like '{:posts ?posts}
      on-success - fn of response data
      on-error - fn of error map {:code :forbidden :reason \"...\" :status 403} or string"
     [pattern on-success on-error]
     (log/log-api-request pattern)
     (-> (js/fetch api-url
                   #js {:method "POST"
                        :headers #js {"Content-Type" "application/transit+json"
                                      "Accept" "application/transit+json"}
                        :credentials "include"
                        :body (encode {:pattern pattern})})
         (.then (fn [resp]
                  ;; Always read body - errors have structured data
                  (-> (.text resp)
                      (.then (fn [text]
                               {:status (.-status resp)
                                :ok (.-ok resp)
                                :body (when (seq text) (decode text))})))))
         (.then (fn [{:keys [status ok body]}]
                  (if ok
                    (do (log/log-api-response body)
                        (on-success body))
                    ;; Extract structured error from response
                    (let [error (if-let [err (first (:errors body))]
                                  (assoc err :status status)
                                  {:code :unknown :reason (str "HTTP " status) :status status})]
                      (log/log-api-error error pattern)
                      (on-error error)))))
         (.catch (fn [err]
                   (let [msg (log/error->string err)]
                     (log/log-api-error msg pattern)
                     (on-error {:code :network :reason msg :status 0})))))))

(def ^:private root-key :app/flybot)

#?(:cljs (defonce app-db (atom {root-key db/initial-state})))

;;=============================================================================
;; Scroll
;;=============================================================================

(def ^:private scroll-keys [:view :tag-filter :author-filter :selected-id])

(defn- should-scroll-top?
  "Check if navigation warrants scrolling to top."
  [old-state new-state]
  (not= (select-keys old-state scroll-keys)
        (select-keys new-state scroll-keys)))

^:rct/test
(comment
  ;; different view → true
  (should-scroll-top? {:view :list :tag-filter nil :author-filter nil :selected-id nil}
                      {:view :detail :tag-filter nil :author-filter nil :selected-id 1})
  ;=> true

  ;; same scroll keys → false
  (should-scroll-top? {:view :list :tag-filter "Home" :author-filter nil :selected-id nil}
                      {:view :list :tag-filter "Home" :author-filter nil :selected-id nil})
  ;=> false

  ;; different tag → true
  (should-scroll-top? {:view :list :tag-filter "Home" :author-filter nil :selected-id nil}
                      {:view :list :tag-filter "clojure" :author-filter nil :selected-id nil})
  ;=> true

  ;; different author → true
  (should-scroll-top? {:view :list :tag-filter nil :author-filter nil :selected-id nil}
                      {:view :list :tag-filter nil :author-filter {:slug "bob"} :selected-id nil}))
  ;=> true)

;;=============================================================================
;; Dispatch
;;=============================================================================

(def ^:private effect-order
  "Execution order for effect types.
   :db       — state update (other effects see updated state)
   :confirm  — blocking dialog, may trigger sub-dispatch
   :pull     — async API call (reads current db snapshot)
   :history  — pushState (reads current db for URL)
   :toast    — add notification (reads toast-counter from db)
   :navigate — hard redirect (last — browser leaves page)"
  [:db :confirm :pull :history :toast :navigate])

#?(:cljs
   (defn dispatch-of
     "Create a stable dispatch function that routes effect maps.
      Returns a single closure with a self-reference via volatile —
      no closure recreation on :confirm, :pull, or :toast.
      Effects execute in `effect-order` — not map iteration order."
     [app-db root-key]
     (let [self (volatile! nil)
           dispatch!
           (fn [effects]
             (doseq [type effect-order
                     :let [effect-def (get effects type)]
                     :when (some? effect-def)]
               (case type
                 :db       (swap! app-db update root-key effect-def)
                 :confirm  (swap! app-db update root-key
                                  assoc :confirm-dialog effect-def)
                 :pull     (let [spec (if (and (map? effect-def) (:pattern effect-def))
                                        effect-def
                                        (pull/resolve-pull effect-def (get @app-db root-key)))]
                             (when-let [{:keys [pattern then]} spec]
                               (pull! pattern
                                      (fn [r] (@self (if (fn? then) (then r) then)))
                                      (fn [e] (@self {:db #(db/set-error % e)})))))
                 :history  (history/push-state! (get @app-db root-key))
                 :toast    (let [id (inc (:toast-counter (get @app-db root-key)))]
                             (@self {:db #(db/add-toast % (:type effect-def) (:title effect-def) (:message effect-def))})
                             (js/setTimeout #(@self {:db (fn [d] (db/remove-toast d id))}) 4000))
                 :navigate (set! (.-location js/window) effect-def))))]
       (vreset! self dispatch!)
       dispatch!)))

#?(:cljs (def dispatch! (dispatch-of app-db root-key)))

#?(:cljs
   (defn ^:export render!
     "Trigger re-render after hot reload."
     []
     (swap! app-db identity)))

;;=============================================================================
;; Rendering
;;=============================================================================

#?(:cljs
   (add-watch app-db :render
              (fn [_ _ old-state new-state]
                (let [old-db (root-key old-state)
                      new-db (root-key new-state)]
                  (when-let [el (js/document.getElementById "app")]
                    (r/render el (views/app-view {:sg.flybot.flybot-site.ui.core.views/db new-db
                                                  :sg.flybot.flybot-site.ui.core.views/dispatch! dispatch!})))
                  (when-not (= (:view old-db) (:view new-db))
                    (log/log-state-change "watcher" old-db new-db))
                  (when (should-scroll-top? old-db new-db)
                    (js/window.scrollTo 0 0))))))

;;=============================================================================
;; Navigation
;;=============================================================================

#?(:cljs
   (defn- on-popstate [parsed]
     (let [{:keys [view id tag author]} parsed]
       (case view
         :list    (dispatch! {:db #(assoc % :view :list :tag-filter tag :author-filter author)})
         :detail  (dispatch! {:db #(db/select-post-start % id) :pull :select-post})
         :edit    (dispatch! {:db #(db/set-view % :edit id)})
         :new     (dispatch! {:db db/view-new})
         :history (dispatch! {:db #(-> (db/select-post-start % id) (assoc :view :history))
                              :pull :select-post})
         :history-detail (dispatch! {:db #(-> (db/select-post-start % id) (assoc :view :history-detail))
                                     :pull :select-post})
         :profile (dispatch! {:db db/view-profile-start :pull :fetch-profile})
         nil))))

#?(:cljs
   (defn- init-from-url! []
     (when-let [parsed (history/path->state (history/current-path))]
       (on-popstate parsed)
       (history/replace-state! (get @app-db root-key)))))

;;=============================================================================
;; Initialization
;;=============================================================================

;; Theme toggle - pure DOM/localStorage, triggers re-render for icon update
#?(:cljs
   (defn ^:export toggle-theme! []
     (let [body (.-body js/document)
           current (.getAttribute body "data-theme")
           new-theme (if (= current "dark") "light" "dark")]
       (.setAttribute body "data-theme" new-theme)
       (js/localStorage.setItem "theme" new-theme)
       (swap! app-db identity))))

;; Export post - pure browser download, no state change
#?(:cljs
   (defn ^:export export-post! [title content]
     (let [filename (str (or title "untitled") ".md")
           blob (js/Blob. #js [content] #js {:type "text/markdown"})
           url (js/URL.createObjectURL blob)
           a (js/document.createElement "a")]
       (set! (.-href a) url)
       (set! (.-download a) filename)
       (.click a)
       (js/URL.revokeObjectURL url)
       (log/info "Exported:" filename))))

#?(:cljs
   (defn- init-theme! []
     (let [saved (js/localStorage.getItem "theme")]
       (when (= saved "dark")
         (.setAttribute (.-body js/document) "data-theme" "dark")))))

;; Dev helper - test error display from browser console: testError("forbidden")
#?(:cljs
   (defn ^:export test-error!
     ([] (test-error! "forbidden"))
     ([error-type]
      (let [errors {"forbidden" {:code :forbidden :reason "You don't own this post" :status 403}
                    "not-found" {:code :not-found :reason "Post not found" :status 404}
                    "network"   {:code :network :reason "Failed to fetch" :status 0}
                    "unknown"   {:code :unknown :reason "Something went wrong" :status 500}}]
        (dispatch! {:db #(db/set-error % (get errors error-type (get errors "unknown")))})))))

;; Dev helper - test toast from browser console: testToast("success", "Done!")
#?(:cljs
   (defn ^:export test-toast!
     ([] (test-toast! "success" "Test Toast"))
     ([toast-type title] (test-toast! toast-type title nil))
     ([toast-type title message]
      (dispatch! {:toast {:type (keyword toast-type) :title title :message message}}))))

#?(:cljs
   (defn ^:export init! []
     (log/info "Flybot site initializing...")
     (history/init-history! on-popstate)
     (init-theme!)
     (init-from-url!)
     (dispatch! {:db db/set-loading :pull :init})
     (log/info "Flybot site initialized")))

#?(:cljs (defonce _init (init!)))
