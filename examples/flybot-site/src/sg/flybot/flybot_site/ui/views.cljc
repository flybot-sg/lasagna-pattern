(ns sg.flybot.flybot-site.ui.views
  "UI views — defalias components with namespaced props.

   Components receive data via ::keys and dispatch effect maps directly."
  (:require [clojure.string :as str]
            [sg.flybot.flybot-site.ui.db :as db]
            [replicant.alias :refer [defalias]]
            #?(:cljs [sg.flybot.flybot-site.ui.api :as api])
            #?(:cljs ["marked" :refer [marked]])
            #?(:cljs ["@toast-ui/editor" :as toastui])))

;;=============================================================================
;; Helpers
;;=============================================================================

(defn format-date [d]
  #?(:clj (when d (str d))
     :cljs (when d (.toLocaleDateString (js/Date. d)))))

(defn tag-list
  "Render tags. If dispatch! provided, tags are clickable to filter."
  ([tags] (tag-list tags nil))
  ([tags dispatch!]
   (when (seq tags)
     [:div.tags
      (for [tag tags]
        [:span {:replicant/key tag
                :class "tag"
                :style (when dispatch! {:cursor "pointer"})
                :on (when dispatch!
                      {:click (fn [e]
                                (.stopPropagation e)
                                (dispatch! {:db #(db/filter-by-tag % tag) :history :push}))})}
         tag])])))

(defn- unescape-markdown [content]
  #?(:clj content
     :cljs (if (string? content)
             (.replace content (js/RegExp. "\\\\([.()\\[\\]])" "g") "$1")
             content)))

(defn render-markdown [content]
  (let [body (-> content db/strip-frontmatter unescape-markdown)]
    #?(:clj [:pre body]
       :cljs (when (seq body)
               [:div {:innerHTML (marked body)}]))))

(defn- markdown->text
  "Convert markdown to plain text by rendering to HTML then extracting text.
   Uses DOM to properly decode HTML entities."
  [s]
  #?(:clj s
     :cljs (let [div (js/document.createElement "div")]
             (set! (.-innerHTML div) (marked s))
             (-> (.-textContent div)
                 (str/replace #"\s+" " ")
                 str/trim))))

(defn- content-preview [content n]
  (let [body (-> (or content "") db/strip-frontmatter markdown->text)]
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

(defn- lock-icon []
  [:svg {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:rect {:x "3" :y "11" :width "18" :height "11" :rx "2" :ry "2"}]
   [:path {:d "M7 11V7a5 5 0 0 1 10 0v4"}]])

(defn- x-circle-icon []
  [:svg {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:circle {:cx "12" :cy "12" :r "10"}]
   [:line {:x1 "15" :y1 "9" :x2 "9" :y2 "15"}]
   [:line {:x1 "9" :y1 "9" :x2 "15" :y2 "15"}]])

(defn- check-circle-icon []
  [:svg.toast-icon {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:path {:d "M22 11.08V12a10 10 0 1 1-5.93-9.14"}]
   [:polyline {:points "22 4 12 14.01 9 11.01"}]])

(defn- info-icon []
  [:svg.toast-icon {:width "20" :height "20" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:circle {:cx "12" :cy "12" :r "10"}]
   [:line {:x1 "12" :y1 "16" :x2 "12" :y2 "12"}]
   [:line {:x1 "12" :y1 "8" :x2 "12.01" :y2 "8"}]])

(defn- alert-icon
  "Select appropriate icon based on error type."
  [error-type]
  (case error-type
    :forbidden (lock-icon)
    :not-found (x-circle-icon)
    (warning-icon)))

(defn- alert-class
  "Determine CSS class based on error type."
  [error]
  (let [{:keys [type retryable?]} error]
    (cond
      retryable? "alert-warning"
      (= type :forbidden) "alert-danger"
      (= type :not-found) "alert-warning"
      (#{:invalid-mutation :schema-violation} type) "alert-warning"
      :else "alert-danger")))

(defn error-banner
  "Display an error banner with appropriate styling based on error type.

   Error types:
   - :forbidden  -> danger (red) with lock icon
   - :not-found  -> warning (yellow) with X icon
   - :network, :execution-error -> warning with retry button
   - other       -> danger (red) with warning icon"
  [db dispatch!]
  (when-let [error (:error db)]
    [:div.alert-box {:class (alert-class error)}
     [:div.alert-content
      (alert-icon (:type error))
      [:span.alert-message (:message error)]]
     (when (:retryable? error)
       [:button.alert-action
        {:on {:click (fn [_] (dispatch! {:db #(-> % db/clear-error db/set-loading) :pull :init}))}}
        "Retry"])
     [:button.alert-dismiss {:on {:click #(dispatch! {:db db/clear-error})}
                             :title "Dismiss"}
      "\u00d7"]]))

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

(defn markdown-editor [content dispatch! editor-key]
  #?(:clj [:textarea {:value content}]
     :cljs [:div.editor-container
            {:replicant/key editor-key
             :replicant/on-mount
             (fn [{:keys [replicant/node replicant/remember]}]
               (remember {:editor (init-editor! node content
                                                #(dispatch! {:db (fn [db] (db/update-form db :content %))}))
                          :content content}))
             :replicant/on-unmount
             (fn [{:keys [replicant/remembered]}]
               (when-let [editor (:editor remembered)]
                 (.destroy editor)))}]))

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

(defn- menu-icon []
  [:svg {:width "24" :height "24" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:line {:x1 "3" :y1 "12" :x2 "21" :y2 "12"}]
   [:line {:x1 "3" :y1 "6" :x2 "21" :y2 "6"}]
   [:line {:x1 "3" :y1 "18" :x2 "21" :y2 "18"}]])

(defn- close-icon []
  [:svg {:width "24" :height "24" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:line {:x1 "18" :y1 "6" :x2 "6" :y2 "18"}]
   [:line {:x1 "6" :y1 "6" :x2 "18" :y2 "18"}]])

(defn- edit-icon []
  [:svg {:width "16" :height "16" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:path {:d "M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"}]
   [:path {:d "M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z"}]])

(defalias mobile-nav-drawer
  "Mobile navigation drawer - slides in from right on mobile."
  [{::keys [db dispatch!]}]
  (let [{:keys [tag-filter mobile-nav-open?]} db
        page-mode? (db/page-mode? db)]
    [:div.mobile-nav-drawer {:class (when mobile-nav-open? "open")}
     [:div.mobile-nav-header
      [:span "Menu"]
      [:button.icon-btn {:title "Close menu"
                         :on {:click #(dispatch! {:db db/close-mobile-nav})}}
       (close-icon)]]
     [:nav.mobile-nav-links
      ;; Pages in defined order
      (for [p db/page-order]
        [:a.mobile-nav-link {:replicant/key p
                             :class (when (and page-mode? (= tag-filter p)) "active")
                             :href "#"
                             :on {:click (fn [e]
                                           (.preventDefault e)
                                           (dispatch! {:db #(db/filter-by-tag % p) :history :push}))}}
         p])
      ;; Posts tab
      [:a.mobile-nav-link {:class (when (not page-mode?) "active")
                           :href "#"
                           :on {:click (fn [e]
                                         (.preventDefault e)
                                         (dispatch! {:db #(db/filter-by-tag % nil) :history :push}))}}
       "Posts"]]]))

(defalias mobile-nav-overlay
  "Overlay behind mobile nav drawer - click to close."
  [{::keys [db dispatch!]}]
  (let [{:keys [mobile-nav-open?]} db]
    [:div.mobile-nav-overlay {:class (when mobile-nav-open? "open")
                              :on {:click #(dispatch! {:db db/close-mobile-nav})}}]))

(defn- header-nav
  "Navigation in header - pages in order, then Posts."
  [db dispatch!]
  (let [{:keys [tag-filter]} db
        page-mode? (db/page-mode? db)]
    [:nav.header-nav
     ;; Pages in defined order
     (for [p db/page-order]
       [:a.nav-link {:replicant/key p
                     :class (when (and page-mode? (= tag-filter p)) "active")
                     :href "#"
                     :on {:click (fn [e]
                                   (.preventDefault e)
                                   (dispatch! {:db #(db/filter-by-tag % p) :history :push}))}}
        p])
     ;; Posts tab
     [:a.nav-link {:class (when (not page-mode?) "active")
                   :href "#"
                   :on {:click (fn [e]
                                 (.preventDefault e)
                                 (dispatch! {:db #(db/filter-by-tag % nil) :history :push}))}}
      "Posts"]]))

(defalias site-header
  "Site header with logo, navigation, theme toggle, and user auth."
  [{::keys [db dispatch!]}]
  (let [user (:user db)]
    [:header.site-header
     [:a.logo {:href "#" :on {:click (fn [e] (.preventDefault e) (dispatch! {:db #(db/filter-by-tag % "Home") :history :push}))}}
      [:img {:src "/assets/flybot-logo.png" :alt "Flybot"}]]
     [:div.header-right
      (header-nav db dispatch!)
      [:button.icon-btn.theme-toggle {:title "Toggle theme"
                                      :on {:click #?(:clj identity
                                                     :cljs #(js/sg.flybot.flybot_site.ui.core.toggle_theme_BANG_))}}
       [:span.show-light (moon-icon)]
       [:span.show-dark (sun-icon)]]
      (if user
        [:div.user-info
         [:a.avatar-link {:href "/profile"
                          :on {:click (fn [e]
                                        (.preventDefault e)
                                        (dispatch! {:db db/view-profile-start :pull :fetch-profile :history :push}))}}
          (if-let [picture (:picture user)]
            [:img.avatar {:src picture :alt (:name user)}]
            ;; Fallback: show initials when no picture
            [:div.avatar.avatar-initials
             (let [name (:name user "?")]
               (-> name (subs 0 1) .toUpperCase))])]
         [:span.user-name (:name user)]
         [:button.icon-btn {:title "Sign out"
                            :on {:click (fn [_] (dispatch! {:db #(assoc % :user nil) :navigate "/logout"}))}}
          (logout-icon)]]
        [:a.icon-btn {:href "/oauth2/google" :title "Sign in with Google"}
         (login-icon)])
      ;; Mobile menu button (visible only on mobile via CSS)
      [:button.icon-btn.mobile-menu-btn {:title "Open menu"
                                         :on {:click #(dispatch! {:db db/toggle-mobile-nav})}}
       (menu-icon)]]]))

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

(defn- author-link
  "Clickable author name that links to /author/:slug."
  [author dispatch!]
  (let [{:user/keys [name slug]} author]
    (if (and name slug dispatch!)
      [:a.author-link {:href (str "/author/" slug)
                       :on {:click (fn [e]
                                     (.preventDefault e)
                                     (.stopPropagation e)
                                     (dispatch! {:db #(db/filter-by-author % {:slug slug :name name}) :history :push}))}}
       name]
      [:span name])))

(defn- page-badges
  "Render page memberships as clickable badges."
  ([post-pages] (page-badges post-pages nil))
  ([post-pages dispatch!]
   (when (seq post-pages)
     [:div.tags.page-badges
      (for [p post-pages]
        [:span.tag.tag-page {:replicant/key p
                             :style (when dispatch! {:cursor "pointer"})
                             :on (when dispatch!
                                   {:click (fn [e]
                                             (.stopPropagation e)
                                             (dispatch! {:db #(db/filter-by-tag % p) :history :push}))})}
         p])])))

(defn- featured-icon []
  [:svg.featured-icon {:width "16" :height "16" :viewBox "0 0 24 24" :fill "currentColor" :stroke "none"
                       :title "Featured post"}
   [:polygon {:points "12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26"}]])

(defn post-card
  "Post card. In page mode: minimal (no metadata, no height limit).
   Shows edit button for author/admin users."
  [{:keys [post dispatch! db page-mode?]}]
  (let [{:post/keys [id title author created-at content tags pages featured?]} post
        can-edit? (db/can-edit-post? db post)]
    (if page-mode?
      [:div.page-card {:on {:click (fn [_] (dispatch! {:db #(db/select-post-start % id) :pull :select-post :history :push}))}}
       [:div.card-header
        [:h2.post-title title]
        (when can-edit?
          [:button.edit-btn {:title "Edit"
                             :on {:click (fn [e]
                                           (.stopPropagation e)
                                           (dispatch! {:db #(db/edit-from-full-post % post) :history :push}))}}
           (edit-icon)])]
       [:div.post-tags-row
        (tag-list tags dispatch!)
        (page-badges pages dispatch!)]
       (when content
         [:div.page-content (render-markdown content)])]
      [:div.post-card {:on {:click (fn [_] (dispatch! {:db #(db/select-post-start % id) :pull :select-post :history :push}))}}
       [:div.card-header
        [:h2.post-title title]
        (when can-edit?
          [:button.edit-btn {:title "Edit"
                             :on {:click (fn [e]
                                           (.stopPropagation e)
                                           (dispatch! {:db #(db/edit-from-full-post % post) :history :push}))}}
           (edit-icon)])]
       [:div.post-meta "By " (author-link author dispatch!) " \u2022 " (format-date created-at)
        (when featured?
          [:a.featured-link {:href "/featured"
                             :title "View all featured posts"
                             :on {:click (fn [e]
                                           (.preventDefault e)
                                           (.stopPropagation e)
                                           (dispatch! {:db #(db/filter-by-tag % "featured") :history :push}))}}
           " \u2022 " (featured-icon)])]
       [:div.post-tags-row
        (tag-list tags dispatch!)
        (page-badges pages dispatch!)]
       (when content
         [:div.post-content (render-markdown content)])])))

(defn slide-card
  "Compact card for slideshow display."
  [post dispatch!]
  (let [{:post/keys [id title author created-at content]} post]
    [:div.slide-card {:replicant/key id
                      :on {:click (fn [_] (dispatch! {:db #(db/select-post-start % id) :pull :select-post :history :push}))}}
     [:h4 title]
     [:div.slide-preview (content-preview content 100)]
     [:div.slide-meta "By " (author-link author dispatch!) " \u2022 " (format-date created-at)]]))

(defn slideshow
  "Horizontal slideshow of posts with navigation."
  [posts dispatch!]
  (when (seq posts)
    [:div.slideshow-container
     [:div.slideshow-header
      [:h3 "More Posts"]
      [:div.slideshow-nav
       [:button.slideshow-btn {:title "Scroll left"
                               :on {:click #?(:cljs #(when-let [track (.querySelector js/document ".slideshow-track")]
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

(defalias post-list-view [{::keys [db dispatch!]}]
  (let [{:keys [loading? tag-filter author-filter]} db
        posts (db/filtered-posts db)
        can-edit? (db/can-edit? db)
        page-mode? (db/page-mode? db)
        author-mode? (db/author-mode? db)
        ;; Derive author name from posts if not in filter (e.g., navigated via URL)
        author-name (or (:name author-filter)
                        (when author-mode?
                          (get-in (first posts) [:post/author :user/name])))
        ;; In page mode: featured post is hero, rest go to slideshow (sorted by date)
        hero-post (when page-mode? (db/hero-post posts))
        slideshow-posts (when page-mode? (db/non-hero-posts posts))]
    [:div (cond-> {} page-mode? (assoc :class "page-view"))
     [:div.list-header
      [:h1.page-title (cond
                        page-mode? tag-filter
                        author-mode? (str "Posts by " (or author-name (:slug author-filter)))
                        :else "Blog Posts")]
      (when can-edit?
        [:button {:on {:click #(dispatch! {:db db/view-new :history :push})}} "New Post"])]
     ;; Show author filter chip
     (when author-mode?
       [:div.tag-filter {:style {:margin-bottom "1rem"}}
        [:span.tag {:style {:cursor "pointer"}
                    :on {:click (fn [_] (dispatch! {:db #(db/filter-by-author % nil) :history :push}))}}
         (or author-name (:slug author-filter)) " \u00d7"]])
     ;; Show tag filter chip only for non-page filters
     (when (and tag-filter (not page-mode?) (not author-mode?))
       [:div.tag-filter {:style {:margin-bottom "1rem"}}
        [:span.tag {:style {:cursor "pointer"}
                    :on {:click (fn [_] (dispatch! {:db #(db/filter-by-tag % nil) :history :push}))}}
         tag-filter " \u00d7"]])
     (when (:error db) (error-banner db dispatch!))
     (if loading?
       [:div.loading "Loading..."]
       (if page-mode?
         ;; Page mode: Hero card + slideshow
         [:div
          (when hero-post
            [:div.page-hero
             [:div {:replicant/key (:post/id hero-post)}
              (post-card {:post hero-post :dispatch! dispatch! :db db :page-mode? true})]])
          (slideshow slideshow-posts dispatch!)]
         ;; Normal mode: list of cards
         [:div.posts
          (for [post posts]
            [:div {:replicant/key (:post/id post)}
             (post-card {:post post :dispatch! dispatch! :db db})])]))]))

(defalias post-detail-view [{::keys [db dispatch!]}]
  (let [post (:selected-post db)
        history-count (count (:history db))
        can-edit? (db/can-edit? db)
        logged-in? (db/logged-in? db)]
    (if post
      [:div.post-detail
       [:div.detail-header
        [:a.back-link {:href "#"
                       :on {:click (fn [e]
                                     (.preventDefault e)
                                     (dispatch! {:db #(db/set-view % :list) :history :push}))}}
         "\u2190 Back to posts"]
        ;; History requires member role
        (when logged-in?
          [:button.secondary
           (cond-> {:on {:click #(dispatch! {:db db/view-history :history :push})}}
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
              history-count])])]
       [:h1 (:post/title post)]
       [:div.post-meta "By " (author-link (:post/author post) dispatch!) " \u2022 " (format-date (:post/created-at post))
        (when (:post/featured? post)
          [:a.featured-link {:href "/featured"
                             :title "View all featured posts"
                             :on {:click (fn [e]
                                           (.preventDefault e)
                                           (dispatch! {:db #(db/filter-by-tag % "featured") :history :push}))}}
           " \u2022 " (featured-icon)])]
       [:div.post-tags-row
        (tag-list (:post/tags post) dispatch!)
        (page-badges (:post/pages post) dispatch!)]
       [:div.post-body (render-markdown (:post/content post))]
       (when can-edit?
         [:div.button-group
          [:button {:on {:click (fn [_] (dispatch! {:db #(db/edit-from-full-post % post) :history :push}))}} "Edit"]
          #?(:cljs [:button {:on {:click #(js/sg.flybot.flybot_site.ui.core.export_post_BANG_
                                           (:post/title post) (:post/content post))}} "Export"])
          [:button.danger {:on {:click #(dispatch! {:confirm {:message "Delete this post?"
                                                              :on-confirm {:db db/set-loading
                                                                           :pull [:delete-post (:post/id post)]}}})}}
           "Delete"]])]
      (if (:loading? db)
        [:div.loading "Loading..."]
        [:div
         [:a.back-link {:href "#"
                        :on {:click (fn [e]
                                      (.preventDefault e)
                                      (dispatch! {:db #(db/set-view % :list) :history :push}))}}
          "\u2190 Back to posts"]
         [:p "Post not found"]]))))

(defalias post-form-view [{::keys [db dispatch!]}]
  (let [{:keys [form error view selected-id]} db
        editing? (= view :edit)
        ;; Unique key per view+post ensures editor remounts on navigation
        editor-key (if editing? (str "edit-" selected-id) "new")]
    [:div.post-form
     [:a.back-link {:href "#"
                    :on {:click (fn [e]
                                  (.preventDefault e)
                                  (dispatch! {:db #(db/set-view % :list) :history :push}))}}
      "\u2190 Cancel"]
     [:h2 (if editing? "Edit Post" "New Post")]
     (when error (error-banner db dispatch!))
     [:div.form-group
      [:label "Title"]
      [:input {:type "text" :value (:title form)
               :on {:input #(dispatch! {:db (fn [db] (db/update-form db :title (.. % -target -value)))})}}]]
     [:div.form-group
      [:label "Pages"]
      [:div.checkbox-group
       (for [p db/page-order]
         [:label.checkbox-label {:replicant/key p}
          [:input {:type "checkbox"
                   :checked (contains? (:pages form) p)
                   :on {:change #(dispatch! {:db (fn [db] (db/toggle-form-page db p))})}}]
          p])]]
     [:div.form-group
      [:label "Tags"]
      [:input {:type "text"
               :value (:tags form)
               :placeholder "clojure, web (comma-separated)"
               :on {:input #(dispatch! {:db (fn [db] (db/update-form db :tags (.. % -target -value)))})}}]]
     [:div.form-group
      [:label.checkbox-label
       [:input {:type "checkbox"
                :checked (:featured? form)
                :on {:change #(dispatch! {:db (fn [db] (db/update-form db :featured? (.. % -target -checked)))})}}]
       "Featured (hero post on page)"]]
     [:div.form-group
      [:label "Content"]
      (markdown-editor (:content form) dispatch! editor-key)]
     [:button {:on {:click #(dispatch! {:db db/set-loading
                                        :pull (if editing? :update-post :create-post)})}}
      (if editing? "Save Changes" "Create Post")]]))

(defalias post-history-view [{::keys [db dispatch!]}]
  (let [post (:selected-post db)
        history (:history db)]
    [:div.post-history
     [:a.back-link {:href "#"
                    :on {:click (fn [e]
                                  (.preventDefault e)
                                  (dispatch! {:db #(db/set-view % :detail) :history :push}))}}
      "\u2190 Back to post"]
     [:h1 "Post History: \"" (:post/title post) "\""]
     (if (empty? history)
       [:p "No history available for this post."]
       [:div.history-table-wrap
        [:table.history-table
         [:thead
          [:tr [:th "Version"] [:th "Date"] [:th "Title"] [:th "Preview"]]]
         [:tbody
          (map-indexed
           (fn [idx version]
             [:tr {:replicant/key (:version/tx version)
                   :on {:click (fn [_] (dispatch! {:db #(db/view-version % version) :history :push}))}}
              [:td (if (zero? idx) "Current" (str "v" (- (count history) idx)))]
              [:td (format-date (:version/timestamp version))]
              [:td (:post/title version)]
              [:td.preview (content-preview (:post/content version) 50)]])
           history)]]])]))

(defalias post-history-detail-view [{::keys [db dispatch!]}]
  (let [version (:history-version db)
        is-current? (= (:version/tx version)
                       (:version/tx (first (:history db))))
        can-edit? (db/can-edit? db)]
    [:div.post-history-detail
     [:a.back-link {:href "#"
                    :on {:click (fn [e]
                                  (.preventDefault e)
                                  (dispatch! {:db #(db/set-view % :history) :history :push}))}}
      "\u2190 Back to history"]
     [:h1 (:post/title version) " "
      [:span.version-label (if is-current? "(Current)" (str "(from " (format-date (:version/timestamp version)) ")"))]]
     [:div.post-meta "By " (author-link (:post/author version) dispatch!) " \u2022 " (format-date (:version/timestamp version))]
     [:div.post-tags-row
      (tag-list (:post/tags version))
      (page-badges (:post/pages version) dispatch!)]
     [:div.post-body.history-content (render-markdown (:post/content version))]
     (when (and can-edit? (not is-current?))
       [:div.button-group {:style {:margin-top "2rem"}}
        [:button {:on {:click (fn [_] (dispatch! {:db #(db/edit-from-full-post % version) :history :push}))}} "Edit This Version"]
        [:button {:on {:click #(dispatch! {:confirm {:message "Restore this version? Current content will be overwritten."
                                                     :on-confirm {:db db/set-loading
                                                                  :pull [:restore-version version]}}})}}
         "Restore This Version"]])]))

;;=============================================================================
;; Profile View
;;=============================================================================

(defn- shield-icon []
  [:svg {:width "16" :height "16" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:path {:d "M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z"}]])

(defn- admin-user-row
  "Row for a user in the admin management table.
   :user/roles is a set of keywords e.g. #{:admin :member :owner}."
  [u dispatch! loading?]
  (let [uid (:user/id u)
        roles (set (:user/roles u))
        is-admin? (contains? roles :admin)
        is-owner? (contains? roles :owner)]
    [:tr {:replicant/key uid}
     [:td (:user/name u)]
     [:td (:user/email u)]
     [:td (for [r (sort (map name roles))]
            [:span.role-badge {:replicant/key r
                               :style {:margin-right "0.25rem"}}
             r])]
     [:td (if is-owner?
            [:span.text-muted "Owner"]
            (if is-admin?
              [:button.btn-sm.btn-danger
               {:disabled loading?
                :on {:click #(dispatch! {:confirm {:message "Revoke admin role from this user?"
                                                   :on-confirm {:db db/set-loading
                                                                :pull [:revoke-admin uid]}}})}}
               "Revoke admin"]
              [:button.btn-sm.btn-primary
               {:disabled loading?
                :on {:click #(dispatch! {:confirm {:message "Grant admin role to this user?"
                                                   :on-confirm {:db db/set-loading
                                                                :pull [:grant-admin uid]}}})}}
               "Grant admin"]))]]))

(defn- admin-management-section
  "Owner-only section for managing admin roles."
  [{:keys [admin-users loading?]} dispatch!]
  (when admin-users
    [:div.profile-admin
     [:h2 (shield-icon) " Admin Management"]
     [:p.text-muted "Grant or revoke admin role. Admins can edit and delete any post."]
     [:table.admin-table
      [:thead [:tr [:th "Name"] [:th "Email"] [:th "Roles"] [:th "Action"]]]
      [:tbody
       (for [u (sort-by :user/name admin-users)]
         (admin-user-row u dispatch! loading?))]]]))

(defalias profile-view [{::keys [db dispatch!]}]
  (let [{:keys [user profile-data]} db
        {:keys [post-count revision-count roles]} profile-data
        owner? (some #(= :owner (:role/name %)) roles)]
    [:div.profile-page
     [:a.back-link {:href "#"
                    :on {:click (fn [e]
                                  (.preventDefault e)
                                  (dispatch! {:db #(db/set-view % :list) :history :push}))}}
      "\u2190 Back to posts"]
     [:div.profile-header
      (if-let [picture (:picture user)]
        [:img.profile-avatar {:src picture :alt (:name user)}]
        [:div.profile-avatar.profile-avatar-initials
         (let [name (:name user "?")]
           (-> name (subs 0 1) .toUpperCase))])
      [:div.profile-info
       [:h1.profile-name (:name user)]
       [:p.profile-email (:email user)]]]
     (if profile-data
       [:div
        [:div.profile-stats
         [:a.profile-stat.profile-stat-link
          {:href (str "/author/" (:slug user))
           :on {:click (fn [e]
                         (.preventDefault e)
                         (dispatch! {:db #(db/filter-by-author % {:slug (:slug user) :name (:name user)}) :history :push}))}}
          [:span.stat-value (or post-count 0)]
          [:span.stat-label "Posts authored"]]
         [:div.profile-stat
          [:span.stat-value (or revision-count 0)]
          [:span.stat-label "Total revisions"]]]
        (when (seq roles)
          [:div.profile-roles
           [:h2 "Roles"]
           [:table.roles-table
            [:thead [:tr [:th "Role"] [:th "Granted"]]]
            [:tbody
             (for [role roles]
               [:tr {:replicant/key (:role/name role)}
                [:td [:span.role-badge (name (:role/name role))]]
                [:td (format-date (:role/granted-at role))]])]]])
        (when owner?
          (admin-management-section db dispatch!))]
       [:div.loading "Loading profile..."])]))

;;=============================================================================
;; Toast Notifications
;;=============================================================================

(defn- toast-icon [type]
  (case type
    :success (check-circle-icon)
    :error [:span.toast-icon (x-circle-icon)]
    :warning [:span.toast-icon (warning-icon)]
    :info (info-icon)
    (info-icon)))

(defn- toast-item [{:keys [id type title message]}]
  [:div.toast {:replicant/key id :class (name type)}
   (toast-icon type)
   [:div.toast-content
    [:div.toast-title title]
    (when message
      [:div.toast-message message])]])

(defn- toast-container [toasts]
  (when (seq toasts)
    [:div.toast-container
     (for [toast toasts]
       (toast-item toast))]))

;;=============================================================================
;; App Root
;;=============================================================================

(defn app-view [{::keys [db dispatch!]}]
  [:div.app-container
   [::site-header {::db db ::dispatch! dispatch!}]
   [::mobile-nav-overlay {::db db ::dispatch! dispatch!}]
   [::mobile-nav-drawer {::db db ::dispatch! dispatch!}]
   [:main.main-content
    (case (:view db)
      :list [::post-list-view {::db db ::dispatch! dispatch!}]
      :detail [::post-detail-view {::db db ::dispatch! dispatch!}]
      :edit [::post-form-view {::db db ::dispatch! dispatch!}]
      :new [::post-form-view {::db db ::dispatch! dispatch!}]
      :history [::post-history-view {::db db ::dispatch! dispatch!}]
      :history-detail [::post-history-detail-view {::db db ::dispatch! dispatch!}]
      :profile [::profile-view {::db db ::dispatch! dispatch!}]
      [::post-list-view {::db db ::dispatch! dispatch!}])]
   (site-footer)
   (toast-container (:toasts db))])

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; post-card normal mode
  (first (post-card {:post {:post/id 1 :post/title "Test" :post/author {:user/name "Me"} :post/content "Hello"} :dispatch! identity :db {}})) ;=> :div.post-card
  ;; post-card page mode
  (first (post-card {:post {:post/id 1 :post/title "Test" :post/content "Hello"} :dispatch! identity :db {} :page-mode? true})) ;=> :div.page-card
  (tag-list []) ;=> nil
  (tag-list [] identity) ;=> nil
  (first (tag-list ["a" "b"])) ;=> :div.tags
  (first (tag-list ["a" "b"] identity)) ;=> :div.tags
  (first (app-view {::db {:view :list :posts [] :loading? false :user nil} ::dispatch! identity})) ;=> :div.app-container
  ;; error-banner renders when error present
  (first (error-banner {:error {:message "Test" :retryable? true :type :network}} identity)) ;=> :div.alert-box
  ;; error-banner returns nil when no error
  (error-banner {:error nil} identity) ;=> nil
  ;; error-banner shows danger style for forbidden
  (second (error-banner {:error {:message "Forbidden" :retryable? false :type :forbidden}} identity)) ;=> {:class "alert-danger"}
  ;; error-banner shows warning style for retryable
  (second (error-banner {:error {:message "Network error" :retryable? true :type :network}} identity)) ;=> {:class "alert-warning"}
  ;; slide-card renders compact card
  (first (slide-card {:post/id 1 :post/title "Test" :post/content "Hello" :post/author {:user/name "Me"}} identity)) ;=> :div.slide-card
  ;; slideshow renders container
  (first (slideshow [{:post/id 1 :post/title "Test"}] identity)) ;=> :div.slideshow-container
  ;; slideshow returns nil for empty posts
  (slideshow [] identity)) ;=> nil
