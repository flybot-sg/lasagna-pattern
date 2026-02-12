(ns sg.flybot.flybot-site.ui.core.history
  "Browser history integration for SPA navigation."
  (:require [sg.flybot.flybot-site.ui.core.db :as db]))

;;=============================================================================
;; Cross-platform helpers
;;=============================================================================

(defn- encode-uri [s]
  #?(:clj  (-> (java.net.URLEncoder/encode (str s) "UTF-8") (.replace "+" "%20"))
     :cljs (js/encodeURIComponent s)))

(defn- decode-uri [s]
  #?(:clj  (java.net.URLDecoder/decode (str s) "UTF-8")
     :cljs (js/decodeURIComponent s)))

(defn- parse-int [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (js/parseInt s 10)))

;;=============================================================================
;; URL <-> State Mapping
;;=============================================================================

(defn state->path
  "Convert app state to URL path."
  [{:keys [view selected-id tag-filter author-filter]}]
  (case view
    :list (cond
            ;; Author filter -> /author/bob-smith
            author-filter
            (str "/author/" (encode-uri (:slug author-filter)))
            ;; Home page -> / (landing page)
            (= tag-filter "Home")
            "/"
            ;; Other page tags -> /page/About
            (and tag-filter (contains? db/pages tag-filter))
            (str "/page/" (encode-uri tag-filter))
            ;; Featured -> /featured
            (= tag-filter "featured")
            "/featured"
            ;; Regular tag -> /tag/clojure
            tag-filter
            (str "/tag/" (encode-uri tag-filter))
            ;; No filter (Posts) -> /posts
            :else "/posts")
    :detail (str "/posts/" selected-id)
    :edit (str "/posts/" selected-id "/edit")
    :new "/posts/new"
    :history (str "/posts/" selected-id "/history")
    :history-detail (str "/posts/" selected-id "/history/detail")
    :profile "/profile"
    "/"))

^:rct/test
(comment
  ;; Home page (tag-filter "Home") -> /
  (state->path {:view :list :tag-filter "Home"})
  ;=> "/"

  ;; Posts list (no filter) -> /posts
  (state->path {:view :list :tag-filter nil})
  ;=> "/posts"

  ;; Tag filter -> /tag/:name
  (state->path {:view :list :tag-filter "clojure"})
  ;=> "/tag/clojure"

  ;; Page filter -> /page/:name
  (state->path {:view :list :tag-filter "About"})
  ;=> "/page/About"

  ;; Featured -> /featured
  (state->path {:view :list :tag-filter "featured"})
  ;=> "/featured"

  ;; Author filter -> /author/:slug
  (state->path {:view :list :author-filter {:slug "bob-smith"}})
  ;=> "/author/bob-smith"

  ;; Detail -> /posts/:id
  (state->path {:view :detail :selected-id 42})
  ;=> "/posts/42"

  ;; Edit -> /posts/:id/edit
  (state->path {:view :edit :selected-id 7})
  ;=> "/posts/7/edit"

  ;; New -> /posts/new
  (state->path {:view :new})
  ;=> "/posts/new"

  ;; History -> /posts/:id/history
  (state->path {:view :history :selected-id 3})
  ;=> "/posts/3/history"

  ;; History detail -> /posts/:id/history/detail
  (state->path {:view :history-detail :selected-id 3})
  ;=> "/posts/3/history/detail"

  ;; Profile -> /profile
  (state->path {:view :profile}))
  ;=> "/profile")

(defn path->state
  "Parse URL path to {:view ... :id ... :tag ... :author ...}. Returns nil for unknown paths."
  [path]
  (let [path (or path "/")]
    (cond
      ;; /posts/new
      (= path "/posts/new")
      {:view :new :id nil}

      ;; /featured - all featured posts
      (= path "/featured")
      {:view :list :id nil :tag "featured"}

      ;; /profile - user profile
      (= path "/profile")
      {:view :profile :id nil}

      ;; /posts - blog posts list
      (= path "/posts")
      {:view :list :id nil :tag nil}

      ;; / or empty - Home page (landing)
      (or (= path "/") (= path ""))
      {:view :list :id nil :tag "Home"}

      :else
      (or
       ;; /author/:slug - author filter
       (when-let [[_ slug] (re-matches #"/author/(.+)" path)]
         {:view :list :id nil :tag nil :author {:slug (decode-uri slug)}})

       ;; /page/:name - pages are just tag filters
       (when-let [[_ tag] (re-matches #"/page/(.+)" path)]
         {:view :list :id nil :tag (decode-uri tag)})

       ;; /tag/:name - regular tag filter
       (when-let [[_ tag] (re-matches #"/tag/(.+)" path)]
         {:view :list :id nil :tag (decode-uri tag)})

       ;; /posts/:id/history/detail (before /history and /posts/:id)
       (when-let [[_ id] (re-matches #"/posts/(\d+)/history/detail" path)]
         {:view :history-detail :id (parse-int id)})

       ;; /posts/:id/history
       (when-let [[_ id] (re-matches #"/posts/(\d+)/history" path)]
         {:view :history :id (parse-int id)})

       ;; /posts/:id/edit
       (when-let [[_ id] (re-matches #"/posts/(\d+)/edit" path)]
         {:view :edit :id (parse-int id)})

       ;; /posts/:id
       (when-let [[_ id] (re-matches #"/posts/(\d+)" path)]
         {:view :detail :id (parse-int id)})

       ;; Unknown path - default to Home
       {:view :list :id nil :tag "Home"}))))

^:rct/test
(comment
  ;; Root -> Home
  (path->state "/")
  ;=> {:view :list :id nil :tag "Home"}

  ;; Posts list
  (path->state "/posts")
  ;=> {:view :list :id nil :tag nil}

  ;; Post detail
  (path->state "/posts/42")
  ;=> {:view :detail :id 42}

  ;; Post edit
  (path->state "/posts/7/edit")
  ;=> {:view :edit :id 7}

  ;; New post
  (path->state "/posts/new")
  ;=> {:view :new :id nil}

  ;; Post history
  (path->state "/posts/3/history")
  ;=> {:view :history :id 3}

  ;; Post history detail
  (path->state "/posts/3/history/detail")
  ;=> {:view :history-detail :id 3}

  ;; Profile
  (path->state "/profile")
  ;=> {:view :profile :id nil}

  ;; Tag filter
  (path->state "/tag/clojure")
  ;=> {:view :list :id nil :tag "clojure"}

  ;; Page filter
  (path->state "/page/About")
  ;=> {:view :list :id nil :tag "About"}

  ;; Featured
  (path->state "/featured")
  ;=> {:view :list :id nil :tag "featured"}

  ;; Author filter
  (path->state "/author/bob-smith")
  ;=> {:view :list :id nil :tag nil :author {:slug "bob-smith"}}

  ;; Unknown path -> Home
  (path->state "/unknown/path")
  ;=> {:view :list :id nil :tag "Home"}

  ;; Round-trip: state->path->state for detail
  (:view (path->state (state->path {:view :detail :selected-id 42})))
  ;=> :detail

  ;; Round-trip: state->path->state for tag
  (:tag (path->state (state->path {:view :list :tag-filter "clojure"}))))
  ;=> "clojure")

;;=============================================================================
;; Browser History API
;;=============================================================================

#?(:cljs
   (defn push-state!
     "Push a new history entry for the given app state."
     [state]
     (let [path (state->path state)]
       (when-not (= path (.-pathname js/location))
         (.pushState js/history (clj->js {:view (:view state)
                                          :id (:selected-id state)
                                          :tag (:tag-filter state)
                                          :author (:author-filter state)})
                     ""
                     path)))))

#?(:cljs
   (defn replace-state!
     "Replace current history entry (use for initial load)."
     [state]
     (let [path (state->path state)]
       (.replaceState js/history (clj->js {:view (:view state)
                                           :id (:selected-id state)
                                           :tag (:tag-filter state)
                                           :author (:author-filter state)})
                      ""
                      path))))

#?(:cljs
   (defn current-path
     "Get current URL path."
     []
     (.-pathname js/location)))

#?(:cljs
   (defn on-popstate!
     "Handle popstate event. Parses current URL and calls on-navigate."
     [on-navigate _e]
     (when-let [parsed (path->state (current-path))]
       (on-navigate parsed))))
