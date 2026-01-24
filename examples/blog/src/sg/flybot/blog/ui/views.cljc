(ns sg.flybot.blog.ui.views
  "UI views - pure functions returning hiccup.

   Views emit events by calling (dispatch! :event) or (dispatch! [:event arg])."
  (:require [sg.flybot.blog.ui.state :as state]
            #?(:cljs [sg.flybot.blog.ui.api :as api])
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
        [:span.tag (cond-> {:replicant/key tag}
                     dispatch! (assoc :on {:click (fn [e]
                                                    (.stopPropagation e)
                                                    (dispatch! [:filter-by-tag tag]))}
                                      :style {:cursor "pointer"}))
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
;; Markdown Editor
;;=============================================================================

#?(:cljs
   (defn- init-editor! [node content on-change]
     (let [Editor (or (.-default toastui) (.-Editor toastui) toastui)
           editor (Editor. #js {:el node
                                :height "400px"
                                :initialEditType "wysiwyg"
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

(defn user-header
  "User header with avatar, name, and login/logout.

   Shows:
   - 'Sign in with Google' link when not logged in
   - User avatar, name, and 'Sign out' link when logged in"
  [state dispatch!]
  (let [user (:user state)]
    [:div.user-header
     (if user
       [:div.user-info
        (when-let [picture (:picture user)]
          [:img.avatar {:src picture :alt (:name user)}])
        [:span.user-name (:name user)]
        [:a.logout-link {:href "#"
                         :on {:click (fn [e]
                                       (.preventDefault e)
                                       (dispatch! :logout))}}
         "Sign out"]]
       [:a.login-btn {:href "/oauth2/google"}
        "Sign in with Google"])]))

(defn post-card [{:post/keys [id title author created-at content tags]} dispatch!]
  [:div.post-card {:on {:click #(dispatch! [:select-post id])}}
   [:h2 title]
   [:div.post-meta "By " author " • " (format-date created-at)]
   [:div.post-content (render-markdown content)]
   (tag-list tags dispatch!)])

(defn post-list-view [{:keys [loading? error tag-filter] :as state} dispatch!]
  (let [posts (state/filtered-posts state)
        can-edit? (state/can-edit? state)]
    [:div
     [:div {:style {:display "flex" :justify-content "space-between" :align-items "center"}}
      [:h1 "Blog Posts"]
      (when can-edit?
        [:button {:on {:click #(dispatch! :view-new)}} "New Post"])]
     (when tag-filter
       [:div.tag-filter {:style {:margin-bottom "1rem"}}
        [:span.tag {:style {:cursor "pointer"}
                    :on {:click #(dispatch! [:filter-by-tag nil])}}
         tag-filter " ×"]])
     (when error [:div.error error])
     (if loading?
       [:div.loading "Loading..."]
       [:div.posts
        (for [post posts]
          [:div {:replicant/key (:post/id post)}
           (post-card post dispatch!)])])]))

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
          [:button.danger {:on {:click #(dispatch! [:delete-post (:post/id post)])}} "Delete"]])]
      [:div
       [:a.back-link {:href "#"
                      :on {:click (fn [e]
                                    (.preventDefault e)
                                    (dispatch! [:view-back :list]))}}
        "← Back to posts"]
       [:p "Post not found"]])))

(defn post-form-view [{:keys [form error view]} dispatch!]
  (let [editing? (= view :edit)]
    [:div.post-form
     [:a.back-link {:href "#"
                    :on {:click (fn [e]
                                  (.preventDefault e)
                                  (dispatch! [:view-back :list]))}}
      "← Cancel"]
     [:h2 (if editing? "Edit Post" "New Post")]
     (when error [:div.error error])
     [:div.form-group
      [:label "Title"]
      [:input {:type "text" :value (:title form)
               :on {:input #(dispatch! [:update-form :title (.. % -target -value)])}}]]
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
  [:div
   (user-header state dispatch!)
   (case (:view state)
     :list (post-list-view state dispatch!)
     :detail (post-detail-view state dispatch!)
     :edit (post-form-view state dispatch!)
     :new (post-form-view state dispatch!)
     :history (post-history-view state dispatch!)
     :history-detail (post-history-detail-view state dispatch!)
     (post-list-view state dispatch!))])

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  (strip-frontmatter "---\nauthor: Bob\n---\n\nHello") ;=> "Hello"
  (strip-frontmatter "***\nauthor: Bob\n***\n\nHello") ;=> "Hello"
  (strip-frontmatter "Just content") ;=> "Just content"
  (strip-frontmatter nil) ;=> ""
  (first (post-card {:post/id 1 :post/title "Test" :post/author "Me" :post/content "Hello"} identity)) ;=> :div.post-card
  (tag-list []) ;=> nil
  (tag-list [] identity) ;=> nil
  (first (tag-list ["a" "b"])) ;=> :div.tags
  (first (tag-list ["a" "b"] identity)) ;=> :div.tags
  (first (app-view {:view :list :posts [] :loading? false :user nil} identity))) ;=> :div)
