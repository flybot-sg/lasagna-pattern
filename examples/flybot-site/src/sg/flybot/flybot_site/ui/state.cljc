(ns sg.flybot.flybot-site.ui.state
  "Application state - pure functions returning {:state ... :fx ...}.

   Each function returns a map with:
   - :state - the new state
   - :fx    - optional effects map {:api ... :confirm ... :history ...}"
  (:require [clojure.string :as str]))

;;=============================================================================
;; State Shape
;;=============================================================================

(def initial-state
  {:view :list        ; :list | :detail | :edit | :new | :history | :history-detail
   :posts []
   :selected-id nil
   :loading? false
   :error nil          ; {:message "..." :retryable? bool} or nil
   :form {:title "" :content "" :tags "" :featured? false}
   :history []
   :history-version nil
   :tag-filter nil    ; nil = show all, string = filter by tag (includes pages)
   :author-filter nil ; nil = show all, {:slug "..." :name "..."} = filter by author
   :pages #{"Home" "About" "Apply"}  ; tags that are pages (get nav tabs, different styling)
   :user nil})        ; {:id :email :name :picture :roles :slug} when logged in

;;=============================================================================
;; Error Classification
;;=============================================================================

(defn- retryable-error-pattern?
  "Check if error string indicates a retryable error (network/server issues)."
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
  "Classify error into {:message ... :retryable? ...}"
  [error]
  (let [raw (cond
              (string? error) error
              (map? error) (or (:message error) (:error error) (str error))
              :else (str error))
        retryable? (retryable-error-pattern? raw)]
    {:message (if retryable?
                "Unable to connect. Please try again."
                (or raw "An error occurred."))
     :retryable? retryable?}))

;;=============================================================================
;; Selectors
;;=============================================================================

(defn selected-post [{:keys [posts selected-id]}]
  (first (filter #(= (:post/id %) selected-id) posts)))

(defn- sort-by-date
  "Sort posts by created-at descending (newest first)."
  [posts]
  (sort-by :post/created-at #(compare %2 %1) posts))

(defn filtered-posts
  "Filter posts for list view.
   - When author-filter is set, show posts by that author
   - When tag-filter is set, show posts with that tag
   - Otherwise, show posts that have at least one non-page tag OR are featured
   - Results sorted by creation date (newest first)"
  [{:keys [posts tag-filter author-filter pages]}]
  (let [pages (or pages #{})]
    (->> (cond
           ;; Author filter takes precedence
           author-filter
           (filter #(= (get-in % [:post/author :user/slug]) (:slug author-filter)) posts)

           ;; Tag filter
           tag-filter
           (filter #(some #{tag-filter} (:post/tags %)) posts)

           ;; Default: show posts with non-page tags or featured
           :else
           (filter #(let [tags (set (:post/tags %))]
                      (or (empty? tags)
                          (:post/featured? %)
                          (not (every? pages tags))))
                   posts))
         sort-by-date)))

(defn hero-post
  "Get the featured post from filtered posts (for page mode hero display)."
  [posts]
  (first (filter :post/featured? posts)))

(defn non-hero-posts
  "Get posts excluding the hero (featured) post."
  [posts]
  (remove :post/featured? posts))

(defn page-mode?
  "Is the current view showing a page (tag-filter is a page tag)?"
  [{:keys [tag-filter pages]}]
  (boolean (and tag-filter (contains? (or pages #{}) tag-filter))))

(defn author-mode?
  "Is the current view showing posts by a specific author?"
  [{:keys [author-filter]}]
  (some? author-filter))

(defn- parse-tags
  "Parse comma-separated tags string into vector."
  [s]
  (when (and (string? s) (seq s))
    (->> (str/split s #",")
         (map str/trim)
         (remove empty?)
         vec)))

(defn- strip-frontmatter
  "Remove YAML frontmatter from content for editing."
  [content]
  (if (and (string? content) (str/starts-with? content "---"))
    (if-let [match (re-find #"(?s)^---\s*\n.*?\n---\s*\n?" content)]
      (str/trim (subs content (count match)))
      content)
    (or content "")))

(defn- has-h1?
  "Check if content has any H1 headings (lines starting with '# ')."
  [content]
  (boolean (and (string? content)
                (re-find #"(?m)^# " content))))

(defn- demote-headings
  "Demote all headings by one level (# -> ##, ## -> ###, etc.)
   Only if content contains H1 headings."
  [content]
  (if (has-h1? content)
    (str/replace content #"(?m)^(#{1,5}) " "$1# ")
    content))

(defn form->post-data [{:keys [form user]}]
  (cond-> {:post/title (:title form)
           :post/content (demote-headings (:content form))
           :post/featured? (boolean (:featured? form))}
    ;; Use user ID for author reference (links to user entity in DB)
    (:id user) (assoc :post/author (:id user))
    (seq (:tags form)) (assoc :post/tags (parse-tags (:tags form)))))

(defn logged-in?
  "Is a user currently logged in?"
  [{:keys [user]}]
  (some? user))

(defn can-edit?
  "Can the current user edit/create/delete posts?"
  [{:keys [user]}]
  (boolean (some #{:member :admin :owner} (:roles user))))

(defn can-edit-post?
  "Can the current user edit this specific post?
   Returns true if user is admin/owner OR the post author."
  [state post]
  (let [user (:user state)
        roles (or (:roles user) #{})
        ;; Author is now a user map with :user/id
        author-id (get-in post [:post/author :user/id])]
    (boolean
     (and user
          (or (some #{:admin :owner} roles)
              (= author-id (:id user)))))))

;;=============================================================================
;; Effect Helpers
;;=============================================================================

(def ^:private fetch-posts-fx
  {:api {:pattern '{:guest {:posts ?posts}}
         :on-success :posts-fetched
         :on-error :error}})

(defn- api-fx [pattern on-success]
  {:api {:pattern pattern :on-success on-success :on-error :error}})

(defn- confirm-fx [message on-confirm]
  {:confirm {:message message :on-confirm on-confirm}})

;;=============================================================================
;; State Transitions (pure functions)
;;=============================================================================

;; --- Loading & Errors ---

(defn set-loading [state]
  {:state (assoc state :loading? true :error nil)})

(defn set-error
  "Set error state with classification."
  [state error]
  {:state (assoc state :loading? false :error (classify-error error))})

(defn clear-error [state]
  {:state (assoc state :error nil)})

;; --- Posts ---

(defn fetch-posts [state]
  {:state (assoc state :loading? true :error nil)
   :fx fetch-posts-fx})

(defn posts-fetched [state posts]
  {:state (assoc state :posts posts :loading? false)})

(defn select-post [state id]
  {:state (-> state
              (assoc :view :detail :selected-id id)
              (cond-> (not= id (:selected-id state))
                (assoc :history [])))
   :fx (merge {:history :push}
              ;; Only fetch history for logged-in users (requires member role)
              (when (logged-in? state)
                (api-fx {:member {:posts/history {{:post/id id} '?versions}}}
                        :history-fetched)))})

;; --- CRUD ---

(defn create-post [state]
  {:state (assoc state :loading? true :error nil)
   :fx (api-fx {:member {:posts {nil (form->post-data state)}}} :post-saved)})

(defn- admin-or-owner? [{:keys [user]}]
  (boolean (some #{:admin :owner} (:roles user))))

(defn update-post [state]
  (let [id (:selected-id state)
        ;; Admins/owners use :admin path (can edit any), members use :member (own only)
        role-key (if (admin-or-owner? state) :admin :member)]
    {:state (assoc state :loading? true :error nil)
     :fx (api-fx {role-key {:posts {{:post/id id} (form->post-data state)}}} :post-saved)}))

(defn post-saved [state _]
  {:state (assoc state :loading? false :view :list :form {:title "" :content "" :tags "" :featured? false})
   :fx (merge {:history :push} fetch-posts-fx)})

(defn delete-post [state id]
  {:state state
   :fx (confirm-fx "Delete this post?" [:delete-confirmed id])})

(defn delete-confirmed [state id]
  (let [role-key (if (admin-or-owner? state) :admin :member)]
    {:state (assoc state :loading? true :error nil)
     :fx (api-fx {role-key {:posts {{:post/id id} nil}}} :post-deleted)}))

(defn post-deleted [state _]
  {:state (assoc state :loading? false :view :list)
   :fx (merge {:history :push} fetch-posts-fx)})

;; --- Views ---

(defn set-view [state view & [id]]
  {:state (-> state
              (assoc :view view)
              (cond-> id (assoc :selected-id id)))
   :fx {:history :push}})

(defn view-new [state]
  {:state (assoc state :view :new :form {:title "" :content "" :tags "" :featured? false})
   :fx {:history :push}})

(defn- tags->string
  "Convert tags vector/set to comma-separated string for form."
  [tags]
  (if (seq tags)
    (str/join ", " tags)
    ""))

(defn view-edit [state post]
  {:state (assoc state
                 :view :edit
                 :selected-id (:post/id post)
                 :form {:title (:post/title post "")
                        :content (strip-frontmatter (:post/content post))
                        :tags (tags->string (:post/tags post))
                        :featured? (boolean (:post/featured? post))})
   :fx {:history :push}})

;; --- Form ---

(defn update-form [state field value]
  {:state (assoc-in state [:form field] value)})

;; --- Tag Filter / Pages ---

(defn filter-by-tag
  "Filter posts by tag. Always navigates to list view with URL update.
   Pages go to /page/{tag}, regular tags go to /tag/{tag}."
  [state tag]
  {:state (assoc state :view :list :tag-filter tag :author-filter nil)
   :fx {:history :push}})

(defn filter-by-author
  "Filter posts by author. Navigates to /user/{slug}."
  [state {:keys [slug name] :as author}]
  {:state (assoc state :view :list :author-filter author :tag-filter nil)
   :fx {:history :push}})

;; --- History ---

(defn view-history [state]
  {:state (assoc state :view :history)
   :fx {:history :push}})

(defn history-fetched [state versions]
  {:state (assoc state :history (or versions []) :loading? false)})

(defn view-version [state version]
  {:state (assoc state :history-version version :view :history-detail)
   :fx {:history :push}})

(defn restore-version [state version]
  {:state state
   :fx (confirm-fx "Restore this version? Current content will be overwritten."
                   [:restore-confirmed version])})

(defn restore-confirmed [state version]
  (let [id (:post/id version)
        role-key (if (admin-or-owner? state) :admin :member)
        data {:post/title (:post/title version)
              :post/content (:post/content version)}]
    {:state (assoc state :loading? true :error nil)
     :fx (api-fx {role-key {:posts {{:post/id id} data}}} :post-saved)}))

;; --- Navigation (browser back/forward) ---

(defn navigate [state {:keys [view id tag author]}]
  (case view
    :list    {:state (assoc state :view :list :tag-filter tag :author-filter author)}
    :detail  {:state (assoc state :view :detail :selected-id id)
              :fx (when (logged-in? state)
                    (api-fx {:member {:posts/history {{:post/id id} '?versions}}}
                            :history-fetched))}
    :edit    (let [post (selected-post (assoc state :selected-id id))]
               {:state (cond-> (assoc state :view :edit :selected-id id)
                         post (assoc :form {:title (:post/title post "")
                                            :content (:post/content post "")}))})
    :new     {:state (assoc state :view :new :form {:title "" :content ""})}
    :history {:state (assoc state :view :history :selected-id id)
              :fx (when (logged-in? state)
                    (api-fx {:member {:posts/history {{:post/id id} '?versions}}}
                            :history-fetched))}
    :history-detail {:state (assoc state :view :history-detail :selected-id id)}
    {:state state}))

;; --- User/Auth ---

(defn fetch-me [state]
  "Fetch current user from :me endpoint.
   Fails silently - guests get 422 which is expected."
  {:state state
   :fx {:api {:pattern '{:member {:me ?user}}
              :on-success :me-fetched
              :on-error :me-fetch-failed}}})

(defn me-fetched [state user]
  "Store user data from :me endpoint response."
  {:state (assoc state :user user)})

(defn me-fetch-failed [state _]
  "Silent handler for auth check failure (expected for guests)."
  {:state state})

(defn logout [state]
  "Clear user and navigate to /logout endpoint."
  {:state (assoc state :user nil)
   :fx {:navigate "/logout"}})

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; fetch-posts sets loading and returns api effect
  (let [{:keys [state fx]} (fetch-posts initial-state)]
    [(:loading? state) (get-in fx [:api :pattern])])
  ;=> [true {:guest {:posts ?posts}}]

  ;; posts-fetched clears loading
  (let [{:keys [state]} (posts-fetched (assoc initial-state :loading? true)
                                       [{:post/id 1}])]
    [(:loading? state) (count (:posts state))])
  ;=> [false 1]

  ;; update-form updates field
  (let [{:keys [state]} (update-form initial-state :title "Hello")]
    (get-in state [:form :title]))
  ;=> "Hello"

  ;; delete-post returns confirm effect
  (let [{:keys [fx]} (delete-post initial-state 1)]
    (get-in fx [:confirm :message]))
  ;=> "Delete this post?"

  ;; set-error clears loading and classifies error
  (let [{:keys [state]} (set-error (assoc initial-state :loading? true) "Oops")]
    [(:loading? state) (:retryable? (:error state))])
  ;=> [false false]

  ;; filtered-posts returns all when no filter
  (filtered-posts {:posts [{:post/id 1}] :tag-filter nil :pages #{}})
  ;=> [{:post/id 1}]

  ;; filtered-posts filters by tag
  (filtered-posts {:posts [{:post/id 1 :post/tags ["clojure"]}
                           {:post/id 2 :post/tags ["java"]}]
                   :tag-filter "clojure"
                   :pages #{}})
  ;=> [{:post/id 1 :post/tags ["clojure"]}]

  ;; filtered-posts excludes page-only posts (unless featured)
  (filtered-posts {:posts [{:post/id 1 :post/tags ["Home"]}
                           {:post/id 2 :post/tags ["clojure"]}
                           {:post/id 3 :post/tags ["Home"] :post/featured? true}]
                   :tag-filter nil
                   :pages #{"Home"}})
  ;=> [{:post/id 2 :post/tags ["clojure"]} {:post/id 3 :post/tags ["Home"] :post/featured? true}]

  ;; page-mode? returns true when tag-filter is a page
  (page-mode? {:tag-filter "Home" :pages #{"Home"}})
  ;=> true

  ;; page-mode? returns false for regular tags
  (page-mode? {:tag-filter "clojure" :pages #{"Home"}})
  ;=> false

  ;; page-mode? returns false when no filter
  (page-mode? {:tag-filter nil :pages #{"Home"}})
  ;=> false

  ;; filter-by-tag with page pushes history
  (let [{:keys [fx]} (filter-by-tag {:pages #{"Home"}} "Home")]
    (:history fx))
  ;=> :push

  ;; filter-by-tag with regular tag now also pushes history (goes to /tag/clojure)
  (let [{:keys [fx]} (filter-by-tag {:pages #{"Home"}} "clojure")]
    (:history fx))
  ;=> :push

  ;; filter-by-tag sets filter and switches to list view
  (let [{:keys [state]} (filter-by-tag {:view :detail} "clojure")]
    [(:view state) (:tag-filter state)])
  ;=> [:list "clojure"]

  ;; filter-by-tag with nil clears filter
  (let [{:keys [state]} (filter-by-tag {:tag-filter "clojure"} nil)]
    (:tag-filter state))
  ;=> nil

  ;; --- Author Filter Tests ---

  ;; author-mode? returns false when no author filter
  (author-mode? {:author-filter nil})
  ;=> false

  ;; author-mode? returns true when author filter set
  (author-mode? {:author-filter {:slug "bob-smith" :name "Bob Smith"}})
  ;=> true

  ;; filter-by-author sets author filter and clears tag filter
  (let [{:keys [state]} (filter-by-author {:tag-filter "clojure"} {:slug "bob-smith" :name "Bob Smith"})]
    [(:author-filter state) (:tag-filter state)])
  ;=> [{:slug "bob-smith" :name "Bob Smith"} nil]

  ;; filtered-posts with author filter
  (filtered-posts {:posts [{:post/id 1 :post/author {:user/slug "bob-smith"}}
                           {:post/id 2 :post/author {:user/slug "jane-doe"}}]
                   :author-filter {:slug "bob-smith"}
                   :tag-filter nil
                   :pages #{}})
  ;=> [{:post/id 1 :post/author {:user/slug "bob-smith"}}]

  ;; author filter takes precedence over tag filter
  (filtered-posts {:posts [{:post/id 1 :post/author {:user/slug "bob-smith"} :post/tags ["clojure"]}
                           {:post/id 2 :post/author {:user/slug "jane-doe"} :post/tags ["clojure"]}]
                   :author-filter {:slug "bob-smith"}
                   :tag-filter "clojure"
                   :pages #{}})
  ;=> [{:post/id 1 :post/author {:user/slug "bob-smith"} :post/tags ["clojure"]}]

  ;; logged-in? returns false for nil user
  (logged-in? {:user nil})
  ;=> false

  ;; logged-in? returns true when user present
  (logged-in? {:user {:email "test@example.com"}})
  ;=> true

  ;; can-edit? returns false for no roles
  (can-edit? {:user {:roles #{}}})
  ;=> false

  ;; can-edit? returns true for member role
  (can-edit? {:user {:roles #{:member}}})
  ;=> true

  ;; me-fetched stores user data
  (let [{:keys [state]} (me-fetched initial-state {:email "test@example.com" :roles #{:owner}})]
    (:email (:user state)))
  ;=> "test@example.com"

  ;; logout clears user and returns navigate effect
  (let [{:keys [state fx]} (logout {:user {:email "test@example.com"}})]
    [(:user state) (:navigate fx)])
  ;=> [nil "/logout"]

  ;; can-edit-post? returns false when not logged in
  (can-edit-post? {:user nil} {:post/author {:user/id "alice-123" :user/name "Alice"}})
  ;=> false

  ;; can-edit-post? returns true for admin role
  (can-edit-post? {:user {:roles #{:admin} :id "admin-123"}} {:post/author {:user/id "alice-123"}})
  ;=> true

  ;; can-edit-post? returns true when user is author (matching user ID)
  (can-edit-post? {:user {:roles #{:member} :id "alice-123"}} {:post/author {:user/id "alice-123"}})
  ;=> true

  ;; can-edit-post? returns false when member is not the author
  (can-edit-post? {:user {:roles #{:member} :id "bob-456"}} {:post/author {:user/id "alice-123"}})
  ;=> false

  ;; --- Error Classification Tests ---

  ;; classify-error detects retryable errors (network)
  (:retryable? (classify-error "Failed to fetch"))
  ;=> true

  ;; classify-error detects retryable errors (server)
  (:retryable? (classify-error "HTTP 500"))
  ;=> true

  ;; classify-error marks other errors as non-retryable
  (:retryable? (classify-error "Something weird"))
  ;=> false

  ;; classify-error provides message
  (some? (:message (classify-error "Failed to fetch")))
  ;=> true

  ;; set-error uses classification
  (let [{:keys [state]} (set-error initial-state "Failed to fetch")]
    (:retryable? (:error state))))
  ;=> true)
