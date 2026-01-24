(ns sg.flybot.blog.ui.state
  "Application state - pure data manipulation.")

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
   :history []              ; list of historical versions
   :history-version nil})   ; currently viewing version

;;=============================================================================
;; State Transitions (pure functions)
;;=============================================================================

(defn set-loading [state loading?]
  (assoc state :loading? loading? :error nil))

(defn set-error [state error]
  (assoc state :loading? false :error (str error)))

(defn set-posts [state posts]
  (assoc state :posts posts :loading? false))

(defn set-view [state view & [post-id]]
  (cond-> (assoc state :view view :selected-id post-id)
    ;; Reset history when changing post
    (not= post-id (:selected-id state))
    (assoc :history [])))

(defn update-form [state field value]
  (assoc-in state [:form field] value))

(defn reset-form [state]
  (assoc state :form {:title "" :content ""}))

(defn set-form-from-post [state post]
  (assoc state :form
         {:title (:post/title post "")
          :content (:post/content post "")}))

(defn set-history [state history]
  (assoc state :history (or history []) :loading? false))

(defn set-history-version [state version]
  (assoc state :history-version version))

;;=============================================================================
;; Selectors (pure functions)
;;=============================================================================

(defn selected-post [state]
  (let [{:keys [posts selected-id]} state]
    (first (filter #(= (:post/id %) selected-id) posts))))

(defn form->post-data [state]
  (let [{:keys [title content]} (:form state)]
    {:post/title title
     :post/content content}))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  (set-loading initial-state true)
  ;=> {:view :list, :posts [], :selected-id nil, :loading? true, :error nil, :form {:title "", :content ""}, :history [], :history-version nil}

  (-> initial-state
      (set-posts [{:id 1 :title "Test"}])
      :posts
      count)
  ;=> 1

  ;; form->post-data extracts title and content
  (-> initial-state
      (assoc :form {:title "T" :content "C"})
      form->post-data))
  ;=> {:post/title "T", :post/content "C"})
