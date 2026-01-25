(ns sg.flybot.flybot-site.ui.state
  "Application state - pure functions returning {:state ... :fx ...}.

   Each function returns a map with:
   - :state - the new state
   - :fx    - optional effects map {:api ... :confirm ... :history ...}")

;;=============================================================================
;; State Shape
;;=============================================================================

(def initial-state
  {:view :list        ; :list | :detail | :edit | :new | :history | :history-detail
   :posts []
   :selected-id nil
   :loading? false
   :error nil          ; {:message "..." :retryable? bool} or nil
   :form {:title "" :content ""}
   :history []
   :history-version nil
   :tag-filter nil    ; nil = show all, string = filter by tag (includes pages)
   :pages #{"Home" "About" "Apply"}  ; tags that are pages (get nav tabs, different styling)
   :user nil})        ; {:email :name :picture :role} when logged in

;;=============================================================================
;; Error Classification
;;=============================================================================

(defn- retryable-error-pattern?
  "Check if error string indicates a retryable error (network/server issues)."
  [s]
  (let [lc (some-> s clojure.string/lower-case)]
    (boolean
     (and lc
          (or (clojure.string/includes? lc "failed to fetch")
              (clojure.string/includes? lc "networkerror")
              (clojure.string/includes? lc "network error")
              (clojure.string/includes? lc "http 5")
              (clojure.string/includes? lc "500"))))))

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

(defn filtered-posts
  "Filter posts for list view.
   - When tag-filter is set, show posts with that tag
   - Otherwise, exclude posts whose ONLY tags are page tags"
  [{:keys [posts tag-filter pages]}]
  (let [pages (or pages #{})]
    (if tag-filter
      (filter #(some #{tag-filter} (:post/tags %)) posts)
      ;; Exclude posts that have ONLY page tags (no regular tags)
      (remove #(let [tags (set (:post/tags %))]
                 (and (seq tags)
                      (every? pages tags)))
              posts))))

(defn page-mode?
  "Is the current view showing a page (tag-filter is a page tag)?"
  [{:keys [tag-filter pages]}]
  (boolean (and tag-filter (contains? (or pages #{}) tag-filter))))

(defn form->post-data [{:keys [form]}]
  {:post/title (:title form)
   :post/content (:content form)})

(defn logged-in?
  "Is a user currently logged in?"
  [{:keys [user]}]
  (some? user))

(defn can-edit?
  "Can the current user edit/create/delete posts?"
  [{:keys [user]}]
  (= :owner (:role user)))

(defn can-edit-post?
  "Can the current user edit this specific post?
   Returns true if user is an admin (owner) OR the post author."
  [state post]
  (let [user (:user state)]
    (boolean
     (and user
          (or (= :owner (:role user))
              (= (:post/author post) (:name user)))))))

;;=============================================================================
;; Effect Helpers
;;=============================================================================

(def ^:private fetch-posts-fx
  {:api {:pattern '{:posts ?posts}
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
              (api-fx {:posts/history {{:post/id id} '?versions}}
                      :history-fetched))})

;; --- CRUD ---

(defn create-post [state]
  {:state (assoc state :loading? true :error nil)
   :fx (api-fx {:posts {nil (form->post-data state)}} :post-saved)})

(defn update-post [state]
  (let [id (:selected-id state)]
    {:state (assoc state :loading? true :error nil)
     :fx (api-fx {:posts {{:post/id id} (form->post-data state)}} :post-saved)}))

(defn post-saved [state _]
  {:state (assoc state :loading? false :view :list :form {:title "" :content ""})
   :fx (merge {:history :push} fetch-posts-fx)})

(defn delete-post [state id]
  {:state state
   :fx (confirm-fx "Delete this post?" [:delete-confirmed id])})

(defn delete-confirmed [state id]
  {:state (assoc state :loading? true :error nil)
   :fx (api-fx {:posts {{:post/id id} nil}} :post-deleted)})

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
  {:state (assoc state :view :new :form {:title "" :content ""})
   :fx {:history :push}})

(defn view-edit [state post]
  {:state (assoc state
                 :view :edit
                 :selected-id (:post/id post)
                 :form {:title (:post/title post "")
                        :content (:post/content post "")})
   :fx {:history :push}})

;; --- Form ---

(defn update-form [state field value]
  {:state (assoc-in state [:form field] value)})

;; --- Tag Filter / Pages ---

(defn filter-by-tag
  "Filter posts by tag. Pages (tags in :pages set) push to history for URL support."
  [state tag]
  (let [is-page? (contains? (or (:pages state) #{}) tag)]
    {:state (assoc state :view :list :tag-filter tag)
     :fx (when (or is-page? (nil? tag)) {:history :push})}))

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
        data {:post/title (:post/title version)
              :post/content (:post/content version)}]
    {:state (assoc state :loading? true :error nil)
     :fx (api-fx {:posts {{:post/id id} data}} :post-saved)}))

;; --- Navigation (browser back/forward) ---

(defn navigate [state {:keys [view id tag]}]
  (case view
    :list    {:state (assoc state :view :list :tag-filter tag)}
    :detail  {:state (assoc state :view :detail :selected-id id)
              :fx (api-fx {:posts/history {{:post/id id} '?versions}}
                          :history-fetched)}
    :edit    (let [post (selected-post (assoc state :selected-id id))]
               {:state (cond-> (assoc state :view :edit :selected-id id)
                         post (assoc :form {:title (:post/title post "")
                                            :content (:post/content post "")}))})
    :new     {:state (assoc state :view :new :form {:title "" :content ""})}
    :history {:state (assoc state :view :history :selected-id id)
              :fx (api-fx {:posts/history {{:post/id id} '?versions}}
                          :history-fetched)}
    :history-detail {:state (assoc state :view :history-detail :selected-id id)}
    {:state state}))

;; --- User/Auth ---

(defn fetch-me [state]
  "Fetch current user from :me endpoint."
  {:state state
   :fx (api-fx '{:me ?user} :me-fetched)})

(defn me-fetched [state user]
  "Store user data from :me endpoint response."
  {:state (assoc state :user user)})

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
  ;=> [true {:posts ?posts}]

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

  ;; filtered-posts excludes page-only posts
  (filtered-posts {:posts [{:post/id 1 :post/tags ["Home"]}
                           {:post/id 2 :post/tags ["clojure"]}
                           {:post/id 3 :post/tags ["Home" "featured"]}]
                   :tag-filter nil
                   :pages #{"Home"}})
  ;=> [{:post/id 2 :post/tags ["clojure"]} {:post/id 3 :post/tags ["Home" "featured"]}]

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

  ;; filter-by-tag with regular tag doesn't push history
  (let [{:keys [fx]} (filter-by-tag {:pages #{"Home"}} "clojure")]
    (:history fx))
  ;=> nil

  ;; filter-by-tag sets filter and switches to list view
  (let [{:keys [state]} (filter-by-tag {:view :detail} "clojure")]
    [(:view state) (:tag-filter state)])
  ;=> [:list "clojure"]

  ;; filter-by-tag with nil clears filter
  (let [{:keys [state]} (filter-by-tag {:tag-filter "clojure"} nil)]
    (:tag-filter state))
  ;=> nil

  ;; logged-in? returns false for nil user
  (logged-in? {:user nil})
  ;=> false

  ;; logged-in? returns true when user present
  (logged-in? {:user {:email "test@example.com"}})
  ;=> true

  ;; can-edit? returns false for viewer role
  (can-edit? {:user {:role :viewer}})
  ;=> false

  ;; can-edit? returns true for owner role
  (can-edit? {:user {:role :owner}})
  ;=> true

  ;; me-fetched stores user data
  (let [{:keys [state]} (me-fetched initial-state {:email "test@example.com" :role :owner})]
    (:email (:user state)))
  ;=> "test@example.com"

  ;; logout clears user and returns navigate effect
  (let [{:keys [state fx]} (logout {:user {:email "test@example.com"}})]
    [(:user state) (:navigate fx)])
  ;=> [nil "/logout"]

  ;; can-edit-post? returns false when not logged in
  (can-edit-post? {:user nil} {:post/author "Alice"})
  ;=> false

  ;; can-edit-post? returns true for owner role (admin)
  (can-edit-post? {:user {:role :owner :name "Admin"}} {:post/author "Alice"})
  ;=> true

  ;; can-edit-post? returns true when user is author
  (can-edit-post? {:user {:role :viewer :name "Alice"}} {:post/author "Alice"})
  ;=> true

  ;; can-edit-post? returns false when viewer is not the author
  (can-edit-post? {:user {:role :viewer :name "Bob"}} {:post/author "Alice"})
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
