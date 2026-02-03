(ns sg.flybot.flybot-site.ui.history
  "Browser history integration for SPA navigation.")

;;=============================================================================
;; URL <-> State Mapping
;;=============================================================================

(defn state->path
  "Convert app state to URL path."
  [{:keys [view selected-id tag-filter author-filter pages]}]
  (case view
    :list (cond
            ;; Author filter -> /author/bob-smith
            author-filter
            (str "/author/" (js/encodeURIComponent (:slug author-filter)))
            ;; Page tag -> /page/Home
            (and tag-filter (contains? (or pages #{}) tag-filter))
            (str "/page/" (js/encodeURIComponent tag-filter))
            ;; Regular tag -> /tag/clojure
            tag-filter
            (str "/tag/" (js/encodeURIComponent tag-filter))
            ;; No filter -> /
            :else "/")
    :detail (str "/posts/" selected-id)
    :edit (str "/posts/" selected-id "/edit")
    :new "/posts/new"
    :history (str "/posts/" selected-id "/history")
    :history-detail (str "/posts/" selected-id "/history/detail")
    "/"))

(defn path->state
  "Parse URL path to {:view ... :id ... :tag ... :author ...}. Returns nil for unknown paths."
  [path]
  (let [path (or path "/")]
    (cond
      ;; /posts/new
      (= path "/posts/new")
      {:view :new :id nil}

      ;; /author/:slug - author filter
      (re-matches #"/author/(.+)" path)
      (let [[_ slug] (re-matches #"/author/(.+)" path)]
        {:view :list :id nil :tag nil :author {:slug (js/decodeURIComponent slug)}})

      ;; /page/:name - pages are just tag filters
      (re-matches #"/page/(.+)" path)
      (let [[_ tag] (re-matches #"/page/(.+)" path)]
        {:view :list :id nil :tag (js/decodeURIComponent tag)})

      ;; /tag/:name - regular tag filter
      (re-matches #"/tag/(.+)" path)
      (let [[_ tag] (re-matches #"/tag/(.+)" path)]
        {:view :list :id nil :tag (js/decodeURIComponent tag)})

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
      {:view :list :id nil :tag nil}

      ;; Unknown path - default to list
      :else
      {:view :list :id nil :tag nil})))

;;=============================================================================
;; Browser History API
;;=============================================================================

(defn push-state!
  "Push a new history entry for the given app state."
  [state]
  (let [path (state->path state)]
    (when (not= path (.-pathname js/location))
      (.pushState js/history (clj->js {:view (:view state)
                                       :id (:selected-id state)
                                       :tag (:tag-filter state)
                                       :author (:author-filter state)})
                  ""
                  path))))

(defn replace-state!
  "Replace current history entry (use for initial load)."
  [state]
  (let [path (state->path state)]
    (.replaceState js/history (clj->js {:view (:view state)
                                        :id (:selected-id state)
                                        :tag (:tag-filter state)
                                        :author (:author-filter state)})
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
