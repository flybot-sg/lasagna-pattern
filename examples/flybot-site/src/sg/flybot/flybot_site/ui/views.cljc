(ns sg.flybot.flybot-site.ui.views
  "UI views - pure functions returning hiccup.

   Views emit events by calling (dispatch! :event) or (dispatch! [:event arg])."
  (:require [sg.flybot.flybot-site.ui.state :as state]
            #?(:cljs [sg.flybot.flybot-site.ui.api :as api])
            #?(:cljs ["marked" :refer [marked]])
            #?(:cljs ["@toast-ui/editor" :as toastui])))

;;=============================================================================
;; Helpers
;;=============================================================================

(defn format-date [d]
  #?(:clj (when d (str d))
     :cljs (when d (.toLocaleDateString (js/Date. d)))))

(def ^:private page-tags
  "Tags that represent pages (styled differently)."
  #{"Home" "About" "Apply"})

(defn tag-list
  "Render tags. If dispatch! provided, tags are clickable to filter.
   Page tags (Home, About, Apply) are styled with .tag-page class."
  ([tags] (tag-list tags nil))
  ([tags dispatch!]
   (when (seq tags)
     [:div.tags
      (for [tag tags
            :let [is-page? (contains? page-tags tag)]]
        [:span {:replicant/key tag
                :class (if is-page? "tag tag-page" "tag")
                :style (when dispatch! {:cursor "pointer"})
                :on (when dispatch!
                      {:click (fn [e]
                                (.stopPropagation e)
                                (dispatch! [:filter-by-tag tag]))})}
         tag])])))

(defn- strip-frontmatter [content]
  (if (and (string? content) (re-find #"^(?:---|\*\*\*)\s*\n" content))
    (if-let [match (re-find #"(?s)^(?:---|\*\*\*)\s*\n.*?\n(?:---|\*\*\*)\s*\n?" content)]
      (subs content (count match))
      content)
    (or content "")))

(defn- unescape-markdown [content]
  #?(:clj content
     :cljs (if (string? content)
             (.replace content (js/RegExp. "\\\\([.()\\[\\]])" "g") "$1")
             content)))

(defn render-markdown [content]
  (let [body (-> content strip-frontmatter unescape-markdown)]
    #?(:clj [:pre body]
       :cljs (when (seq body)
               [:div {:innerHTML (marked body)}]))))

(defn- content-preview [content n]
  (let [body (strip-frontmatter (or content ""))]
    (if (> (count body) n)
      (str (subs body 0 n) "...")
      body)))

;;=============================================================================
;; Slideshow Icons
;;=============================================================================

(defn- chevron-left-icon []
  [:svg {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:polyline {:points "15 18 9 12 15 6"}]])

(defn- chevron-right-icon []
  [:svg {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:polyline {:points "9 18 15 12 9 6"}]])

;;=============================================================================
;; Error Components
;;=============================================================================

(defn- warning-icon []
  [:svg {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:path {:d "M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"}]
   [:line {:x1 "12" :y1 "9" :x2 "12" :y2 "13"}]
   [:line {:x1 "12" :y1 "17" :x2 "12.01" :y2 "17"}]])

(defn error-banner
  "Display an error banner with retry button for retryable errors."
  [state dispatch!]
  (when-let [error (:error state)]
    [:div.error-banner {:class (if (:retryable? error) "retryable" "error")}
     (warning-icon)
     [:span (:message error)]
     (when (:retryable? error)
       [:button {:on {:click #(do (dispatch! :clear-error)
                                  (dispatch! :fetch-posts))}}
        "Retry"])]))

;;=============================================================================
;; Markdown Editor
;;=============================================================================

#?(:cljs
   (defn- init-editor! [node content on-change]
     (let [Editor (or (.-default toastui) (.-Editor toastui) toastui)
           editor (Editor. #js {:el node
                                :height "400px"
                                :initialEditType "markdown"
                                :previewStyle "vertical"
                                :initialValue (or content "")
                                :hooks #js {:addImageBlobHook
                                            (fn [blob callback]
                                              (-> (api/upload-image! blob)
                                                  (.then #(callback % (.-name blob))))
                                              false)}})]
       (.on editor "change" #(on-change (.getMarkdown editor)))
       editor)))

(defn markdown-editor [content dispatch!]
  #?(:clj [:textarea {:value content}]
     :cljs [:div.editor-container
            {:replicant/on-mount
             (fn [{:keys [replicant/node replicant/remember]}]
               (remember {:editor (init-editor! node content
                                                #(dispatch! [:update-form :content %]))
                          :content content}))}]))

;;=============================================================================
;; Components
;;=============================================================================

(defn- login-icon []
  [:svg {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:path {:d "M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4"}]
   [:polyline {:points "10 17 15 12 10 7"}]
   [:line {:x1 "15" :y1 "12" :x2 "3" :y2 "12"}]])

(defn- logout-icon []
  [:svg {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:path {:d "M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"}]
   [:polyline {:points "16 17 21 12 16 7"}]
   [:line {:x1 "21" :y1 "12" :x2 "9" :y2 "12"}]])

(defn- sun-icon []
  [:svg {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:circle {:cx "12" :cy "12" :r "5"}]
   [:line {:x1 "12" :y1 "1" :x2 "12" :y2 "3"}]
   [:line {:x1 "12" :y1 "21" :x2 "12" :y2 "23"}]
   [:line {:x1 "4.22" :y1 "4.22" :x2 "5.64" :y2 "5.64"}]
   [:line {:x1 "18.36" :y1 "18.36" :x2 "19.78" :y2 "19.78"}]
   [:line {:x1 "1" :y1 "12" :x2 "3" :y2 "12"}]
   [:line {:x1 "21" :y1 "12" :x2 "23" :y2 "12"}]
   [:line {:x1 "4.22" :y1 "19.78" :x2 "5.64" :y2 "18.36"}]
   [:line {:x1 "18.36" :y1 "5.64" :x2 "19.78" :y2 "4.22"}]])

(defn- moon-icon []
  [:svg {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:path {:d "M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"}]])

(defn- edit-icon []
  [:svg {:width "16" :height "16" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:path {:d "M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"}]
   [:path {:d "M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z"}]])

(def ^:private page-order ["Home" "About" "Apply"])

(defn- header-nav
  "Navigation in header - pages in order, then Posts."
  [{:keys [tag-filter pages] :as state} dispatch!]
  (let [pages-set (or pages #{})
        page-mode? (state/page-mode? state)]
    [:nav.header-nav
     ;; Pages in defined order
     (for [p page-order
           :when (contains? pages-set p)]
       [:a.nav-link {:replicant/key p
                     :class (when (and page-mode? (= tag-filter p)) "active")
                     :href "#"
                     :on {:click (fn [e]
                                   (.preventDefault e)
                                   (dispatch! [:filter-by-tag p]))}}
        p])
     ;; Posts tab
     [:a.nav-link {:class (when (not page-mode?) "active")
                   :href "#"
                   :on {:click (fn [e]
                                 (.preventDefault e)
                                 (dispatch! [:filter-by-tag nil]))}}
      "Posts"]]))

(defn site-header
  "Site header with logo, navigation, theme toggle, and user auth."
  [state dispatch!]
  (let [user (:user state)]
    [:header.site-header
     [:a.logo {:href "#" :on {:click (fn [e] (.preventDefault e) (dispatch! [:filter-by-tag "Home"]))}}
      [:img {:src "/assets/flybot-logo.png" :alt "Flybot"}]]
     [:div.header-right
      (header-nav state dispatch!)
      [:button.icon-btn.theme-toggle {:title "Toggle theme"
                                      :on {:click #?(:clj identity
                                                     :cljs #(js/sg.flybot.flybot_site.ui.core.toggle_theme_BANG_))}}
       [:span.show-light (moon-icon)]
       [:span.show-dark (sun-icon)]]
      (if user
        [:div.user-info
         (when-let [picture (:picture user)]
           [:img.avatar {:src picture :alt (:name user)}])
         [:span.user-name (:name user)]
         [:button.icon-btn {:title "Sign out"
                            :on {:click #(dispatch! :logout)}}
          (logout-icon)]]
        [:a.icon-btn {:href "/oauth2/google" :title "Sign in with Google"}
         (login-icon)])]]))

(defn site-footer []
  [:footer.site-footer
   [:div.footer-content
    [:div.footer-section
     [:h4 "Address"]
     [:p "1 Commonwealth Lane" [:br]
      "#08-14" [:br]
      "One Commonwealth" [:br]
      "Singapore 149544"]]
    [:div.footer-section
     [:h4 "Business Hours"]
     [:p "Monday - Friday" [:br]
      "08:30 - 17:00"]]
    [:div.footer-section
     [:h4 "Contact"]
     [:p [:a {:href "mailto:zhengliming@basecity.com"} "zhengliming@basecity.com"]]
     [:p [:a {:href "https://www.linkedin.com/company/flybot-pte-ltd" :target "_blank"} "LinkedIn"]]]]])

(defn post-card
  "Post card. In page mode: minimal (no metadata, no height limit).
   Shows edit button for author/admin users."
  [post dispatch! state & [page-mode?]]
  (let [{:post/keys [id title author created-at content tags]} post
        can-edit? (state/can-edit-post? state post)]
    (if page-mode?
      [:div.page-card {:on {:click #(dispatch! [:select-post id])}}
       [:div.card-header
        [:h2.post-title title]
        (when can-edit?
          [:button.edit-btn {:title "Edit"
                             :on {:click (fn [e]
                                           (.stopPropagation e)
                                           (dispatch! [:view-edit post]))}}
           (edit-icon)])]
       [:div.page-content (render-markdown content)]]
      [:div.post-card {:on {:click #(dispatch! [:select-post id])}}
       [:div.card-header
        [:h2.post-title title]
        (when can-edit?
          [:button.edit-btn {:title "Edit"
                             :on {:click (fn [e]
                                           (.stopPropagation e)
                                           (dispatch! [:view-edit post]))}}
           (edit-icon)])]
       [:div.post-meta "By " author " • " (format-date created-at)]
       [:div.post-content (render-markdown content)]
       (tag-list tags dispatch!)])))

(defn slide-card
  "Compact card for slideshow display."
  [post dispatch!]
  (let [{:post/keys [id title author created-at content]} post]
    [:div.slide-card {:replicant/key id
                      :on {:click #(dispatch! [:select-post id])}}
     [:h4 title]
     [:div.slide-preview (content-preview content 100)]
     [:div.slide-meta "By " author " • " (format-date created-at)]]))

(defn slideshow
  "Horizontal slideshow of posts with navigation."
  [posts dispatch!]
  (when (seq posts)
    [:div.slideshow-container
     [:div.slideshow-header
      [:h3 "More Posts"]
      [:div.slideshow-nav
       [:button.slideshow-btn {:title "Scroll left"
                               :on {:click #?(:cljs #(when-let [track (.-previousElementSibling (.-currentTarget %))]
                                                       (set! (.-scrollLeft track)
                                                             (- (.-scrollLeft track) 300)))
                                              :clj identity)}}
        (chevron-left-icon)]
       [:button.slideshow-btn {:title "Scroll right"
                               :on {:click #?(:cljs #(when-let [track (.querySelector js/document ".slideshow-track")]
                                                       (set! (.-scrollLeft track)
                                                             (+ (.-scrollLeft track) 300)))
                                              :clj identity)}}
        (chevron-right-icon)]]]
     [:div.slideshow-track
      (for [post posts]
        (slide-card post dispatch!))]]))

(defn post-list-view [{:keys [loading? error tag-filter] :as state} dispatch!]
  (let [posts (state/filtered-posts state)
        can-edit? (state/can-edit? state)
        page-mode? (state/page-mode? state)
        ;; In page mode: featured post is hero, rest go to slideshow (sorted by date)
        hero-post (when page-mode? (state/hero-post posts))
        slideshow-posts (when page-mode? (state/non-hero-posts posts))]
    [:div (cond-> {} page-mode? (assoc :class "page-view"))
     [:div {:style {:display "flex" :justify-content "space-between" :align-items "center"}}
      [:h1.page-title (if page-mode? tag-filter "Blog Posts")]
      (when can-edit?
        [:button {:on {:click #(dispatch! :view-new)}} "New Post"])]
     ;; Show tag filter chip only for non-page filters
     (when (and tag-filter (not page-mode?))
       [:div.tag-filter {:style {:margin-bottom "1rem"}}
        [:span.tag {:style {:cursor "pointer"}
                    :on {:click #(dispatch! [:filter-by-tag nil])}}
         tag-filter " ×"]])
     (when error (error-banner state dispatch!))
     (if loading?
       [:div.loading "Loading..."]
       (if page-mode?
         ;; Page mode: Hero card + slideshow
         [:div
          (when hero-post
            [:div.page-hero
             [:div {:replicant/key (:post/id hero-post)}
              (post-card hero-post dispatch! state true)]])
          (slideshow slideshow-posts dispatch!)]
         ;; Normal mode: list of cards
         [:div.posts
          (for [post posts]
            [:div {:replicant/key (:post/id post)}
             (post-card post dispatch! state false)])]))]))

(defn post-detail-view [state dispatch!]
  (let [post (state/selected-post state)
        history-count (count (:history state))
        can-edit? (state/can-edit? state)]
    (if post
      [:div.post-detail
       [:div.detail-header
        [:a.back-link {:href "#"
                       :on {:click (fn [e]
                                     (.preventDefault e)
                                     (dispatch! [:view-back :list]))}}
         "← Back to posts"]
        [:button.secondary
         (cond-> {:on {:click #(dispatch! :view-history)}}
           (zero? history-count) (assoc :disabled true
                                        :style {:opacity 0.5 :cursor "not-allowed"}))
         "View History"
         (when (pos? history-count)
           [:span.badge {:style {:margin-left "0.5em"
                                 :background "#666"
                                 :color "#fff"
                                 :padding "0.1em 0.5em"
                                 :border-radius "10px"
                                 :font-size "0.85em"}}
            history-count])]]
       [:h1 (:post/title post)]
       [:div.post-meta "By " (:post/author post) " • " (format-date (:post/created-at post))]
       (tag-list (:post/tags post) dispatch!)
       [:div.post-body (render-markdown (:post/content post))]
       (when can-edit?
         [:div.button-group
          [:button {:on {:click #(dispatch! [:view-edit post])}} "Edit"]
          #?(:cljs [:button {:on {:click #(js/sg.flybot.flybot_site.ui.core.export_post_BANG_
                                           (:post/title post) (:post/content post))}} "Export"])
          [:button.danger {:on {:click #(dispatch! [:delete-post (:post/id post)])}} "Delete"]])]
      [:div
       [:a.back-link {:href "#"
                      :on {:click (fn [e]
                                    (.preventDefault e)
                                    (dispatch! [:view-back :list]))}}
        "← Back to posts"]
       [:p "Post not found"]])))

(defn post-form-view [{:keys [form error view] :as state} dispatch!]
  (let [editing? (= view :edit)]
    [:div.post-form
     [:a.back-link {:href "#"
                    :on {:click (fn [e]
                                  (.preventDefault e)
                                  (dispatch! [:view-back :list]))}}
      "← Cancel"]
     [:h2 (if editing? "Edit Post" "New Post")]
     (when error (error-banner state dispatch!))
     [:div.form-group
      [:label "Title"]
      [:input {:type "text" :value (:title form)
               :on {:input #(dispatch! [:update-form :title (.. % -target -value)])}}]]
     [:div.form-group
      [:label "Tags"]
      [:input {:type "text"
               :value (:tags form)
               :placeholder "clojure, web, Home (comma-separated)"
               :on {:input #(dispatch! [:update-form :tags (.. % -target -value)])}}]
      [:small {:style {:color "var(--text-muted)" :display "block" :margin-top "0.25rem"}}
       "Page tags: Home, About, Apply"]]
     [:div.form-group
      [:label.checkbox-label
       [:input {:type "checkbox"
                :checked (:featured? form)
                :on {:change #(dispatch! [:update-form :featured? (.. % -target -checked)])}}]
       "Featured (hero post on page)"]]
     [:div.form-group
      [:label "Content"]
      (markdown-editor (:content form) dispatch!)]
     [:button {:on {:click #(dispatch! (if editing? :update-post :create-post))}}
      (if editing? "Save Changes" "Create Post")]]))

(defn post-history-view [state dispatch!]
  (let [post (state/selected-post state)
        history (:history state)]
    [:div.post-history
     [:a.back-link {:href "#"
                    :on {:click (fn [e]
                                  (.preventDefault e)
                                  (dispatch! [:view-back :detail]))}}
      "← Back to post"]
     [:h1 "Post History: \"" (:post/title post) "\""]
     (if (empty? history)
       [:p "No history available for this post."]
       [:table.history-table
        [:thead
         [:tr [:th "Version"] [:th "Date"] [:th "Title"] [:th "Preview"]]]
        [:tbody
         (map-indexed
          (fn [idx version]
            [:tr {:replicant/key (:version/tx version)
                  :on {:click #(dispatch! [:view-version version])}}
             [:td (if (zero? idx) "Current" (str "v" (- (count history) idx)))]
             [:td (format-date (:version/timestamp version))]
             [:td (:post/title version)]
             [:td.preview (content-preview (:post/content version) 50)]])
          history)]])]))

(defn post-history-detail-view [state dispatch!]
  (let [version (:history-version state)
        is-current? (= (:version/tx version)
                       (:version/tx (first (:history state))))
        can-edit? (state/can-edit? state)]
    [:div.post-history-detail
     [:a.back-link {:href "#"
                    :on {:click (fn [e]
                                  (.preventDefault e)
                                  (dispatch! [:view-back :history]))}}
      "← Back to history"]
     [:h1 (:post/title version)
      [:span.version-label (if is-current? "(Current)" (str "(from " (format-date (:version/timestamp version)) ")"))]]
     [:div.post-meta "By " (:post/author version) " • " (format-date (:version/timestamp version))]
     (tag-list (:post/tags version))
     [:div.post-body.history-content (render-markdown (:post/content version))]
     (when (and can-edit? (not is-current?))
       [:div.button-group {:style {:margin-top "2rem"}}
        [:button {:on {:click #(dispatch! [:restore-version version])}} "Restore This Version"]])]))

(defn app-view [state dispatch!]
  [:div.app-container
   (site-header state dispatch!)
   [:main.main-content
    (case (:view state)
      :list (post-list-view state dispatch!)
      :detail (post-detail-view state dispatch!)
      :edit (post-form-view state dispatch!)
      :new (post-form-view state dispatch!)
      :history (post-history-view state dispatch!)
      :history-detail (post-history-detail-view state dispatch!)
      (post-list-view state dispatch!))]
   (site-footer)])

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  (strip-frontmatter "---\nauthor: Bob\n---\n\nHello") ;=> "Hello"
  (strip-frontmatter "***\nauthor: Bob\n***\n\nHello") ;=> "Hello"
  (strip-frontmatter "Just content") ;=> "Just content"
  (strip-frontmatter nil) ;=> ""
  ;; post-card normal mode
  (first (post-card {:post/id 1 :post/title "Test" :post/author "Me" :post/content "Hello"} identity {})) ;=> :div.post-card
  ;; post-card page mode
  (first (post-card {:post/id 1 :post/title "Test" :post/content "Hello"} identity {} true)) ;=> :div.page-card
  (tag-list []) ;=> nil
  (tag-list [] identity) ;=> nil
  (first (tag-list ["a" "b"])) ;=> :div.tags
  (first (tag-list ["a" "b"] identity)) ;=> :div.tags
  (first (app-view {:view :list :posts [] :loading? false :user nil :pages #{}} identity)) ;=> :div.app-container
  ;; error-banner renders when error present
  (first (error-banner {:error {:message "Test" :retryable? true}} identity)) ;=> :div.error-banner
  ;; error-banner returns nil when no error
  (error-banner {:error nil} identity) ;=> nil
  ;; slide-card renders compact card
  (first (slide-card {:post/id 1 :post/title "Test" :post/content "Hello" :post/author "Me"} identity)) ;=> :div.slide-card
  ;; slideshow renders container
  (first (slideshow [{:post/id 1 :post/title "Test"}] identity)) ;=> :div.slideshow-container
  ;; slideshow returns nil for empty posts
  (slideshow [] identity)) ;=> nil
