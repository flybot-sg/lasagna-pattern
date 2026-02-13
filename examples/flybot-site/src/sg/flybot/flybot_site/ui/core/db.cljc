(ns sg.flybot.flybot-site.ui.core.db
  "Application state — pure db -> db updater functions.

   Every function takes db (and optional args) and returns the new db.
   No effect maps — effects are declared in views and executed by dispatch!."
  (:require [clojure.string :as str]))

;;=============================================================================
;; State Shape
;;=============================================================================

(def page-order
  "Ordered list of pages for navigation tabs."
  ["Home" "About" "Apply"])

(def pages
  "Set of page names (get nav tabs, hero post, different styling)."
  (set page-order))

(def initial-state
  {:view :list        ; :list | :detail | :edit | :new | :history | :history-detail | :profile
   :posts []          ; full posts from list (content included)
   :selected-id nil
   :selected-post nil ; full post (from :posts cache or detail fetch fallback)
   :loading? false
   :error nil          ; {:message "..." :retryable? bool} or nil
   :form {:title "" :content "" :tags "" :pages #{} :featured? false}
   :history []
   :history-version nil
   :profile-data nil   ; {:post-count :revision-count :roles}
   :admin-users nil    ; [{:user/id :user/name :user/roles ...}] for owner management
   :tag-filter "Home" ; nil = show all posts, string = filter by tag or page
   :author-filter nil ; nil = show all, {:slug "..." :name "..."} = filter by author
   :user nil          ; {:email :name :picture :roles :slug} when logged in
   :mobile-nav-open? false  ; mobile navigation drawer state
   :toasts []         ; [{:id :type :title :message} ...] active toast notifications
   :toast-counter 0  ; auto-increment ID for toasts
   :version nil})    ; app version from meta tag (e.g. "0.1.10")

;;=============================================================================
;; Error Classification
;;=============================================================================

(def ^:private error-messages
  "Human-readable messages for error codes."
  {:forbidden "You don't have permission to do that."
   :not-found "The requested item was not found."
   :invalid-mutation "Invalid operation."
   :already-granted "Role is already assigned."
   :schema-violation "Invalid data format."
   :network "Unable to connect. Please check your connection."
   :unknown "An unexpected error occurred."})

(def ^:private retryable-codes
  "Error codes that indicate retryable errors."
  #{:network :execution-error})

(defn- retryable-status?
  [status]
  (and (number? status) (>= status 500)))

(defn- retryable-error-pattern?
  [s]
  (let [lc (some-> s str/lower-case)]
    (boolean
     (and lc
          (or (str/includes? lc "failed to fetch")
              (str/includes? lc "networkerror")
              (str/includes? lc "network error")
              (str/includes? lc "http 5")
              (str/includes? lc "500"))))))

(defn classify-error
  "Classify error into {:message ... :retryable? ... :type ...}"
  [error]
  (cond
    (and (map? error) (:code error))
    (let [{:keys [code reason status]} error
          retryable? (or (contains? retryable-codes code)
                         (retryable-status? status))]
      {:message (or reason (get error-messages code) (name code))
       :retryable? retryable?
       :type code})

    (string? error)
    (let [retryable? (retryable-error-pattern? error)]
      {:message (if retryable?
                  "Unable to connect. Please try again."
                  error)
       :retryable? retryable?
       :type (if retryable? :network :unknown)})

    (map? error)
    (let [raw (or (:message error) (:error error) (str error))
          retryable? (retryable-error-pattern? raw)]
      {:message (if retryable?
                  "Unable to connect. Please try again."
                  (or raw "An error occurred."))
       :retryable? retryable?
       :type (if retryable? :network :unknown)})

    :else
    {:message "An error occurred."
     :retryable? false
     :type :unknown}))

^:rct/test
(comment
  (classify-error {:code :forbidden :reason "You don't own this post" :status 403})
  ;=> {:message "You don't own this post" :retryable? false :type :forbidden}

  (:message (classify-error {:code :forbidden :status 403}))
  ;=> "You don't have permission to do that."

  (:retryable? (classify-error "Failed to fetch"))
  ;=> true

  (:retryable? (classify-error "Something weird")))
  ;=> false)

;;=============================================================================
;; Selectors
;;=============================================================================

(defn- sort-by-date
  [posts]
  (sort-by :post/created-at #(compare %2 %1) posts))

(defn filtered-posts
  "Filter posts for list view by author, tag, page, or featured."
  [{:keys [posts tag-filter author-filter]}]
  (->> (cond
         author-filter
         (filter #(= (get-in % [:post/author :user/slug]) (:slug author-filter)) posts)

         (= tag-filter "featured")
         (filter :post/featured? posts)

         (and tag-filter (contains? pages tag-filter))
         (filter #(some #{tag-filter} (:post/pages %)) posts)

         tag-filter
         (filter #(some #{tag-filter} (:post/tags %)) posts)

         :else
         posts)
       sort-by-date))

^:rct/test
(comment
  (filtered-posts {:posts [{:post/id 1}] :tag-filter nil})
  ;=> [{:post/id 1}]

  (filtered-posts {:posts [{:post/id 1 :post/tags ["clojure"]}
                           {:post/id 2 :post/tags ["java"]}]
                   :tag-filter "clojure"})
  ;=> [{:post/id 1 :post/tags ["clojure"]}]

  (filtered-posts {:posts [{:post/id 1 :post/pages ["Home"]}
                           {:post/id 2 :post/tags ["clojure"]}]
                   :tag-filter "Home"})
  ;=> [{:post/id 1 :post/pages ["Home"]}]

  (filtered-posts {:posts [{:post/id 1 :post/author {:user/slug "bob"}}
                           {:post/id 2 :post/author {:user/slug "jane"}}]
                   :author-filter {:slug "bob"}
                   :tag-filter nil}))
  ;=> [{:post/id 1 :post/author {:user/slug "bob"}}])

(defn hero-post
  "Get the featured post from filtered posts (for page mode hero display)."
  [posts]
  (first (filter :post/featured? posts)))

(defn non-hero-posts
  "Get posts excluding the hero (featured) post."
  [posts]
  (remove :post/featured? posts))

(defn page-mode?
  "Is the current view showing a page (tag-filter is a page name)?"
  [{:keys [tag-filter]}]
  (boolean (and tag-filter (contains? pages tag-filter))))

^:rct/test
(comment
  (page-mode? {:tag-filter "Home"}) ;=> true
  (page-mode? {:tag-filter "clojure"}) ;=> false
  (page-mode? {:tag-filter nil})) ;=> false)

(defn author-mode?
  "Is the current view showing posts by a specific author?"
  [{:keys [author-filter]}]
  (some? author-filter))

(defn- parse-tags
  [s]
  (when (and (string? s) (seq s))
    (->> (str/split s #",")
         (map str/trim)
         (remove empty?)
         vec)))

(defn strip-frontmatter
  "Remove YAML/TOML frontmatter (--- or ***) from markdown content."
  [content]
  (if (and (string? content) (re-find #"^(?:---|\*\*\*)\s*\n" content))
    (if-let [match (re-find #"(?s)^(?:---|\*\*\*)\s*\n.*?\n(?:---|\*\*\*)\s*\n?" content)]
      (str/trim (subs content (count match)))
      content)
    (or content "")))

^:rct/test
(comment
  (strip-frontmatter "---\nauthor: Bob\n---\n\nHello") ;=> "Hello"
  (strip-frontmatter "***\nauthor: Bob\n***\n\nHello") ;=> "Hello"
  (strip-frontmatter "Just content") ;=> "Just content"
  (strip-frontmatter nil)) ;=> "")

(defn- has-h1?
  [content]
  (boolean (and (string? content)
                (re-find #"(?m)^# " content))))

(defn- demote-headings
  [content]
  (if (has-h1? content)
    (str/replace content #"(?m)^(#{1,5}) " "$1# ")
    content))

(defn- tags->string
  [tags]
  (if (seq tags)
    (str/join ", " tags)
    ""))

(defn form->post-data [{:keys [form]}]
  (cond-> {:post/title (:title form)
           :post/content (demote-headings (:content form))
           :post/featured? (boolean (:featured? form))}
    (seq (:tags form)) (assoc :post/tags (parse-tags (:tags form)))
    (seq (:pages form)) (assoc :post/pages (vec (:pages form)))))

(defn logged-in?
  [{:keys [user]}]
  (some? user))

^:rct/test
(comment
  (logged-in? {:user nil}) ;=> false
  (logged-in? {:user {:email "t@t.com"}})) ;=> true)

(defn can-edit?
  "Can the current user edit/create/delete posts?"
  [{:keys [user]}]
  (boolean (some #{:member :admin :owner} (:roles user))))

^:rct/test
(comment
  (can-edit? {:user {:roles #{}}}) ;=> false
  (can-edit? {:user {:roles #{:member}}})) ;=> true)

(defn can-edit-post?
  "Can the current user edit this specific post?"
  [db post]
  (let [user (:user db)
        roles (or (:roles user) #{})
        author-email (get-in post [:post/author :user/email])]
    (boolean
     (and user
          (or (some #{:admin :owner} roles)
              (= author-email (:email user)))))))

^:rct/test
(comment
  (can-edit-post? {:user nil} {:post/author {:user/email "a@b.com"}}) ;=> false
  (can-edit-post? {:user {:roles #{:admin} :email "x@b.com"}} {:post/author {:user/email "a@b.com"}}) ;=> true
  (can-edit-post? {:user {:roles #{:member} :email "a@b.com"}} {:post/author {:user/email "a@b.com"}}) ;=> true
  (can-edit-post? {:user {:roles #{:member} :email "b@b.com"}} {:post/author {:user/email "a@b.com"}})) ;=> false)

(defn admin-or-owner?
  "Is the current user an admin or owner?"
  [{:keys [user]}]
  (boolean (some #{:admin :owner} (:roles user))))

^:rct/test
(comment
  (admin-or-owner? {:user {:roles #{:admin}}}) ;=> true
  (admin-or-owner? {:user {:roles #{:member}}})) ;=> false)

;;=============================================================================
;; State Transitions (pure db -> db updaters)
;;=============================================================================

;; --- Loading & Errors ---

(defn set-loading [db]
  (assoc db :loading? true :error nil))

^:rct/test
(comment
  (select-keys (set-loading initial-state) [:loading? :error]))
  ;=> {:loading? true :error nil})

(defn set-error [db error]
  (assoc db :loading? false :error (classify-error error)))

^:rct/test
(comment
  (let [db (set-error (assoc initial-state :loading? true) "Oops")]
    [(:loading? db) (:retryable? (:error db))])
  ;=> [false false]

  (:retryable? (:error (set-error initial-state "Failed to fetch"))))
  ;=> true)

(defn clear-error [db]
  (assoc db :error nil))

;; --- Init (combined posts + auth) ---

(defn init-fetched [db result]
  (-> db
      (assoc :posts (get result 'posts) :loading? false)
      (cond-> (get result 'user) (assoc :user (get result 'user)))))

^:rct/test
(comment
  (let [db (init-fetched (assoc initial-state :loading? true)
                         {'posts [{:post/id 1}] 'user {:email "t@t.com"}})]
    [(:loading? db) (count (:posts db)) (:email (:user db))])
  ;=> [false 1 "t@t.com"]

  ;; guest (no user in response)
  (let [db (init-fetched (assoc initial-state :loading? true) {'posts [{:post/id 1}]})]
    [(:loading? db) (:user db)]))
  ;=> [false nil])

;; --- Post Detail ---

(defn select-post-start [db id]
  (let [cached (first (filter #(= (:post/id %) id) (:posts db)))]
    (-> db
        (assoc :view :detail :selected-id id)
        (cond->
         (not= id (:selected-id db)) (assoc :history [])
         cached       (assoc :selected-post cached :loading? false)
         (not cached) (assoc :selected-post nil :loading? true)))))

^:rct/test
(comment
  ;; not cached — loading
  (let [db (select-post-start initial-state 42)]
    [(:view db) (:selected-id db) (:loading? db)])
  ;=> [:detail 42 true]

  ;; cached — shows immediately, no loading
  (let [db (select-post-start (assoc initial-state :posts [{:post/id 42 :post/title "Hi"}]) 42)]
    [(:loading? db) (:post/title (:selected-post db))])
  ;=> [false "Hi"]

  ;; clears stale data when ID changes
  (let [db (select-post-start (assoc initial-state :selected-id 1 :history [:old]) 42)]
    (:history db))
  ;=> []

  ;; preserves history when same ID
  (let [db (select-post-start (assoc initial-state :selected-id 42 :history [:old]) 42)]
    (:history db)))
  ;=> [:old])

(defn post-detail-fetched [db result]
  (-> db
      (assoc :selected-post (get result 'post) :loading? false)
      (assoc :history (or (get result 'versions) []))))

;; --- Form ---

(defn update-form [db field value]
  (assoc-in db [:form field] value))

^:rct/test
(comment
  (get-in (update-form initial-state :title "Hello") [:form :title]))
  ;=> "Hello")

(defn toggle-form-page [db page]
  (update-in db [:form :pages] #(let [ps (or % #{})]
                                  (if (contains? ps page) (disj ps page) (conj ps page)))))

(defn clear-form [db]
  (assoc db :form {:title "" :content "" :tags "" :pages #{} :featured? false}))

^:rct/test
(comment
  (:form (clear-form (assoc initial-state :form {:title "X" :content "Y" :tags "z" :pages #{"Home"} :featured? true}))))
  ;=> {:title "" :content "" :tags "" :pages #{} :featured? false})

;; --- Mutation Responses ---

(defn post-created [db post]
  (-> db
      (update :posts conj post)
      (assoc :view :list :loading? false)
      clear-form))

(defn post-updated [db post]
  (let [id (:post/id post)]
    (-> db
        (update :posts (fn [posts] (mapv #(if (= (:post/id %) id) post %) posts)))
        (assoc :view :list :loading? false)
        clear-form)))

(defn post-deleted [db post-id]
  (-> db
      (update :posts (fn [posts] (filterv #(not= (:post/id %) post-id) posts)))
      (assoc :view :list :loading? false)))

^:rct/test
(comment
  ;; post-created adds to list
  (let [db (post-created {:posts [{:post/id 1}] :loading? true
                          :form {:title "X"} :view :new :toast-counter 0 :toasts []}
                         {:post/id 2 :post/title "New"})]
    [(count (:posts db)) (:post/id (last (:posts db))) (:view db) (:loading? db)])
  ;=> [2 2 :list false]

  ;; post-updated replaces in list
  (let [db (post-updated {:posts [{:post/id 1 :post/title "Old"} {:post/id 2}]
                          :loading? true :form {:title "X"} :view :edit
                          :toast-counter 0 :toasts []}
                         {:post/id 1 :post/title "New"})]
    [(:post/title (first (:posts db))) (:view db)])
  ;=> ["New" :list]

  ;; post-deleted removes from list
  (let [db (post-deleted {:posts [{:post/id 1} {:post/id 2}] :loading? true :view :detail}
                         1)]
    [(count (:posts db)) (:post/id (first (:posts db))) (:view db)]))
  ;=> [1 2 :list])

;; --- Edit ---

(defn edit-from-full-post [db post]
  (assoc db
         :view :edit
         :selected-id (:post/id post)
         :form {:title (:post/title post "")
                :content (strip-frontmatter (:post/content post))
                :tags (tags->string (:post/tags post))
                :pages (set (:post/pages post))
                :featured? (boolean (:post/featured? post))}))

;; --- Views ---

(defn set-view [db view & [id]]
  (-> db
      (assoc :view view)
      (cond-> id (assoc :selected-id id))))

(defn view-new [db]
  (assoc db :view :new :form {:title "" :content "" :tags "" :pages #{} :featured? false}))

;; --- Mobile Navigation ---

(defn toggle-mobile-nav [db]
  (update db :mobile-nav-open? not))

(defn close-mobile-nav [db]
  (assoc db :mobile-nav-open? false))

^:rct/test
(comment
  (:mobile-nav-open? (toggle-mobile-nav {:mobile-nav-open? false})) ;=> true
  (:mobile-nav-open? (toggle-mobile-nav {:mobile-nav-open? true})) ;=> false
  (:mobile-nav-open? (close-mobile-nav {:mobile-nav-open? true}))) ;=> false)

;; --- Tag Filter / Pages ---

(defn filter-by-tag [db tag]
  (assoc db :view :list :tag-filter tag :author-filter nil :mobile-nav-open? false))

^:rct/test
(comment
  (let [db (filter-by-tag {:view :detail} "clojure")]
    [(:view db) (:tag-filter db)])
  ;=> [:list "clojure"]

  (:mobile-nav-open? (filter-by-tag {:mobile-nav-open? true} "Home")))
  ;=> false)

(defn filter-by-author [db author]
  (assoc db :view :list :author-filter author :tag-filter nil))

^:rct/test
(comment
  (let [db (filter-by-author {:tag-filter "clojure"} {:slug "bob" :name "Bob"})]
    [(:author-filter db) (:tag-filter db)]))
  ;=> [{:slug "bob" :name "Bob"} nil])

;; --- History ---

(defn view-history [db]
  (assoc db :view :history))

(defn view-version [db version]
  (assoc db :history-version version :view :history-detail))

;; --- Profile ---

(defn view-profile-start [db]
  (assoc db :view :profile :profile-data nil :admin-users nil))

^:rct/test
(comment
  (let [db (view-profile-start (assoc initial-state :profile-data {:x 1} :admin-users [:u]))]
    [(:view db) (:profile-data db) (:admin-users db)]))
  ;=> [:profile nil nil])

(defn profile-fetched [db result]
  (-> db
      (assoc :profile-data (get result 'profile))
      (cond-> (get result 'users) (assoc :admin-users (get result 'users)))))

;; --- Toast Notifications ---

(defn add-toast [db type title & [message]]
  (let [id (inc (:toast-counter db))
        toast {:id id :type type :title title :message message}]
    (-> db
        (update :toasts conj toast)
        (assoc :toast-counter id))))

^:rct/test
(comment
  (let [db (add-toast {:toasts [] :toast-counter 0} :success "Saved")]
    [(count (:toasts db)) (:id (first (:toasts db))) (:toast-counter db)]))
  ;=> [1 1 1])

(defn remove-toast [db id]
  (update db :toasts (fn [ts] (filterv #(not= (:id %) id) ts))))

^:rct/test
(comment
  (mapv :id (:toasts (remove-toast {:toasts [{:id 1} {:id 2} {:id 3}]} 2))))
  ;=> [1 3])
