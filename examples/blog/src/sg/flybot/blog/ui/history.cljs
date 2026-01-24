(ns sg.flybot.blog.ui.history
  "Browser history integration for SPA navigation.")

;;=============================================================================
;; URL <-> State Mapping
;;=============================================================================

(defn state->path
  "Convert app state to URL path."
  [{:keys [view selected-id]}]
  (case view
    :list "/"
    :detail (str "/posts/" selected-id)
    :edit (str "/posts/" selected-id "/edit")
    :new "/posts/new"
    :history (str "/posts/" selected-id "/history")
    :history-detail (str "/posts/" selected-id "/history/detail")
    "/"))

(defn path->state
  "Parse URL path to {:view ... :id ...}. Returns nil for unknown paths."
  [path]
  (let [path (or path "/")]
    (cond
      ;; /posts/new
      (= path "/posts/new")
      {:view :new :id nil}

      ;; /posts/:id/history/detail
      (re-matches #"/posts/(\d+)/history/detail" path)
      (let [[_ id] (re-matches #"/posts/(\d+)/history/detail" path)]
        {:view :history-detail :id (js/parseInt id 10)})

      ;; /posts/:id/history
      (re-matches #"/posts/(\d+)/history" path)
      (let [[_ id] (re-matches #"/posts/(\d+)/history" path)]
        {:view :history :id (js/parseInt id 10)})

      ;; /posts/:id/edit
      (re-matches #"/posts/(\d+)/edit" path)
      (let [[_ id] (re-matches #"/posts/(\d+)/edit" path)]
        {:view :edit :id (js/parseInt id 10)})

      ;; /posts/:id
      (re-matches #"/posts/(\d+)" path)
      (let [[_ id] (re-matches #"/posts/(\d+)" path)]
        {:view :detail :id (js/parseInt id 10)})

      ;; / or empty
      (or (= path "/") (= path ""))
      {:view :list :id nil}

      ;; Unknown path - default to list
      :else
      {:view :list :id nil})))

;;=============================================================================
;; Browser History API
;;=============================================================================

(defn push-state!
  "Push a new history entry for the given app state."
  [state]
  (let [path (state->path state)]
    (when (not= path (.-pathname js/location))
      (.pushState js/history (clj->js {:view (:view state)
                                       :id (:selected-id state)})
                  ""
                  path))))

(defn replace-state!
  "Replace current history entry (use for initial load)."
  [state]
  (let [path (state->path state)]
    (.replaceState js/history (clj->js {:view (:view state)
                                        :id (:selected-id state)})
                   ""
                   path)))

(defn current-path
  "Get current URL path."
  []
  (.-pathname js/location))

(defn init-history!
  "Initialize history listener. Calls on-navigate when back/forward pressed."
  [on-navigate]
  (.addEventListener js/window "popstate"
                     (fn [_e]
                       (let [parsed (path->state (current-path))]
                         (when parsed
                           (on-navigate parsed))))))
