(ns sg.flybot.blog.ui.state
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
   :error nil
   :form {:title "" :content ""}
   :history []
   :history-version nil})

;;=============================================================================
;; Selectors
;;=============================================================================

(defn selected-post [{:keys [posts selected-id]}]
  (first (filter #(= (:post/id %) selected-id) posts)))

(defn form->post-data [{:keys [form]}]
  {:post/title (:title form)
   :post/content (:content form)})

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

(defn set-error [state error]
  {:state (assoc state :loading? false :error (str error))})

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

(defn navigate [state {:keys [view id]}]
  (case view
    :list    {:state (assoc state :view :list)}
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

  ;; set-error clears loading
  (let [{:keys [state]} (set-error (assoc initial-state :loading? true) "Oops")]
    [(:loading? state) (:error state)]))
  ;=> [false "Oops"])
