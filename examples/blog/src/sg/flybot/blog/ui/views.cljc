(ns sg.flybot.blog.ui.views
  "UI views - pure functions returning hiccup.

   All functions take state and return hiccup data.
   Event handlers are passed as a map of action functions."
  (:require [sg.flybot.blog.ui.state :as state]))

;;=============================================================================
;; Helpers
;;=============================================================================

(defn format-date [d]
  #?(:clj (when d (str d))
     :cljs (when d (.toLocaleDateString (js/Date. d)))))

(defn tag-list [tags]
  (when (seq tags)
    [:div.tags
     (for [tag tags]
       [:span.tag {:replicant/key tag} tag])]))

;;=============================================================================
;; Components
;;=============================================================================

(defn post-card [{:keys [id title author created-at content tags]} actions]
  [:div.post-card {:on {:click #((:on-select actions) id)}}
   [:h2 title]
   [:div.post-meta "By " author " • " (format-date created-at)]
   [:p.post-content
    (if (> (count content) 150)
      (str (subs content 0 150) "...")
      content)]
   (tag-list tags)])

(defn post-list-view [{:keys [posts loading? error]} actions]
  [:div
   [:div {:style {:display "flex" :justify-content "space-between" :align-items "center"}}
    [:h1 "Blog Posts"]
    [:button {:on {:click (:on-new actions)}} "New Post"]]
   (when error [:div.error error])
   (if loading?
     [:div.loading "Loading..."]
     [:div.posts
      (for [post posts]
        [:div {:replicant/key (:id post)}
         (post-card post actions)])])])

(defn post-detail-view [state actions]
  (let [post (state/selected-post state)]
    (if post
      [:div.post-detail
       [:a.back-link {:href "#" :on {:click (:on-back actions)}} "← Back to posts"]
       [:h1 (:title post)]
       [:div.post-meta "By " (:author post) " • " (format-date (:created-at post))]
       (tag-list (:tags post))
       [:div {:style {:margin-top "2rem" :white-space "pre-wrap"}} (:content post)]
       [:div.button-group {:style {:margin-top "2rem"}}
        [:button {:on {:click #((:on-edit actions) post)}} "Edit"]
        [:button.danger {:on {:click #((:on-delete actions) (:id post))}} "Delete"]]]
      [:div
       [:a.back-link {:href "#" :on {:click (:on-back actions)}} "← Back to posts"]
       [:p "Post not found"]])))

(defn post-form-view [{:keys [form error view]} actions]
  (let [editing? (= view :edit)]
    [:div.post-form
     [:a.back-link {:href "#" :on {:click (:on-back actions)}} "← Cancel"]
     [:h2 (if editing? "Edit Post" "New Post")]
     (when error [:div.error error])
     [:div.form-group
      [:label "Title"]
      [:input {:type "text" :value (:title form)
               :on {:input #((:on-field actions) :title %)}}]]
     [:div.form-group
      [:label "Author"]
      [:input {:type "text" :value (:author form)
               :on {:input #((:on-field actions) :author %)}}]]
     [:div.form-group
      [:label "Tags (comma-separated)"]
      [:input {:type "text" :value (:tags form)
               :on {:input #((:on-field actions) :tags %)}}]]
     [:div.form-group
      [:label "Content"]
      [:textarea {:value (:content form)
                  :on {:input #((:on-field actions) :content %)}}]]
     [:button {:on {:click (:on-submit actions)}}
      (if editing? "Save Changes" "Create Post")]]))

(defn app-view [state actions]
  (case (:view state)
    :list (post-list-view state actions)
    :detail (post-detail-view state actions)
    :edit (post-form-view state actions)
    :new (post-form-view state actions)
    (post-list-view state actions)))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; post-card returns hiccup
  (first (post-card {:id 1 :title "Test" :author "Me" :content "Hello"} {}))
  ;=> :div.post-card

  ;; tag-list returns nil for empty
  (tag-list [])
  ;=> nil

  ;; tag-list returns hiccup for tags
  (first (tag-list ["a" "b"]))
  ;=> :div.tags

  ;; app-view dispatches on :view
  (first (app-view {:view :list :posts [] :loading? false} {})))
  ;=> :div)
