(ns sg.flybot.blog.ui.views
  "UI views - pure functions returning hiccup.

   All functions take state and return hiccup data.
   Event handlers are passed as a map of action functions."
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

(defn tag-list [tags]
  (when (seq tags)
    [:div.tags
     (for [tag tags]
       [:span.tag {:replicant/key tag} tag])]))

(defn- strip-frontmatter
  "Remove YAML frontmatter from markdown content.
   Handles both --- and *** delimiters (Toast UI converts --- to ***)."
  [content]
  (if (and (string? content) (re-find #"^(?:---|\*\*\*)\s*\n" content))
    (if-let [match (re-find #"(?s)^(?:---|\*\*\*)\s*\n.*?\n(?:---|\*\*\*)\s*\n?" content)]
      (subs content (count match))
      content)
    (or content "")))

(defn- unescape-markdown
  "Remove backslash escapes that Toast UI adds to URLs (e.g. \\. becomes .)"
  [content]
  #?(:clj content
     :cljs (if (string? content)
             (.replace content (js/RegExp. "\\\\([.()\\[\\]])" "g") "$1")
             content)))

(defn render-markdown
  "Render markdown content as HTML (strips frontmatter, unescapes Toast UI escapes)."
  [content]
  (let [body (-> content strip-frontmatter unescape-markdown)]
    #?(:clj [:pre body]
       :cljs (when (seq body)
               [:div {:innerHTML (marked body)}]))))

(defn- content-preview
  "Get first n chars of content body (without frontmatter)."
  [content n]
  (let [body (strip-frontmatter (or content ""))]
    (if (> (count body) n)
      (str (subs body 0 n) "...")
      body)))

;;=============================================================================
;; Markdown Editor (Toast UI)
;;=============================================================================

#?(:cljs
   (defn- init-editor!
     "Initialize Toast UI Editor on a DOM node."
     [node content on-change]
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

(defn markdown-editor
  "WYSIWYG markdown editor component using Toast UI Editor."
  [content on-change]
  #?(:clj [:textarea {:value content}]
     :cljs [:div.editor-container
            {:replicant/on-mount
             (fn [{:keys [replicant/node replicant/remember]}]
               (remember {:editor (init-editor! node content on-change)
                          :content content}))}]))

;;=============================================================================
;; Components
;;=============================================================================

(defn post-card [{:post/keys [id title author created-at content tags]} actions]
  [:div.post-card {:on {:click #((:on-select actions) id)}}
   [:h2 title]
   [:div.post-meta "By " author " • " (format-date created-at)]
   [:div.post-content (render-markdown content)]
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
        [:div {:replicant/key (:post/id post)}
         (post-card post actions)])])])

(defn post-detail-view [state actions]
  (let [post (state/selected-post state)]
    (if post
      [:div.post-detail
       [:div.detail-header
        [:a.back-link {:href "#" :on {:click #((:on-back actions) % :list)}} "← Back to posts"]
        [:button.secondary {:on {:click #((:on-history actions) (:post/id post))}} "View History"]]
       [:h1 (:post/title post)]
       [:div.post-meta "By " (:post/author post) " • " (format-date (:post/created-at post))]
       (tag-list (:post/tags post))
       [:div.post-body (render-markdown (:post/content post))]
       [:div.button-group
        [:button {:on {:click #((:on-edit actions) post)}} "Edit"]
        [:button.danger {:on {:click #((:on-delete actions) (:post/id post))}} "Delete"]]]
      [:div
       [:a.back-link {:href "#" :on {:click #((:on-back actions) % :list)}} "← Back to posts"]
       [:p "Post not found"]])))

(defn post-form-view [{:keys [form error view]} actions]
  (let [editing? (= view :edit)]
    [:div.post-form
     [:a.back-link {:href "#" :on {:click #((:on-back actions) % :list)}} "← Cancel"]
     [:h2 (if editing? "Edit Post" "New Post")]
     (when error [:div.error error])
     [:div.form-group
      [:label "Title"]
      [:input {:type "text" :value (:title form)
               :on {:input #((:on-field actions) :title %)}}]]
     [:div.form-group
      [:label "Content"]
      (markdown-editor (:content form) (:on-content actions))]
     [:button {:on {:click (:on-submit actions)}}
      (if editing? "Save Changes" "Create Post")]]))

(defn post-history-view
  "List view showing all historical versions of a post."
  [state actions]
  (let [post (state/selected-post state)
        history (:history state)]
    [:div.post-history
     [:a.back-link {:href "#" :on {:click #((:on-back actions) % :detail)}} "← Back to post"]
     [:h1 "Post History: \"" (:post/title post) "\""]
     (if (empty? history)
       [:p "No history available for this post."]
       [:table.history-table
        [:thead
         [:tr
          [:th "Version"] [:th "Date"] [:th "Title"] [:th "Preview"]]]
        [:tbody
         (map-indexed
          (fn [idx version]
            [:tr {:replicant/key (:version/tx version)
                  :on {:click #((:on-view-version actions) version)}}
             [:td (if (zero? idx) "Current" (str "v" (- (count history) idx)))]
             [:td (format-date (:version/timestamp version))]
             [:td (:post/title version)]
             [:td.preview (content-preview (:post/content version) 50)]])
          history)]])]))

(defn post-history-detail-view
  "Read-only view of a historical version with restore option."
  [state actions]
  (let [version (:history-version state)
        is-current? (= (:version/tx version)
                       (:version/tx (first (:history state))))]
    [:div.post-history-detail
     [:a.back-link {:href "#" :on {:click #((:on-back actions) % :history)}} "← Back to history"]
     [:h1 (:post/title version)
      [:span.version-label (if is-current? "(Current)" (str "(from " (format-date (:version/timestamp version)) ")"))]]
     [:div.post-meta "By " (:post/author version) " • " (format-date (:version/timestamp version))]
     (tag-list (:post/tags version))
     [:div.post-body.history-content (render-markdown (:post/content version))]
     (when-not is-current?
       [:div.button-group {:style {:margin-top "2rem"}}
        [:button {:on {:click #((:on-restore actions) version)}} "Restore This Version"]])]))

(defn app-view [state actions]
  (case (:view state)
    :list (post-list-view state actions)
    :detail (post-detail-view state actions)
    :edit (post-form-view state actions)
    :new (post-form-view state actions)
    :history (post-history-view state actions)
    :history-detail (post-history-detail-view state actions)
    (post-list-view state actions)))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; strip-frontmatter removes YAML frontmatter
  (strip-frontmatter "---\nauthor: Bob\n---\n\nHello")
  ;=> "Hello"

  ;; strip-frontmatter handles tags
  (strip-frontmatter "---\nauthor: Alice\ntags:\n  - a\n  - b\n---\n\nContent")
  ;=> "Content"

  ;; strip-frontmatter handles *** delimiters (Toast UI format)
  (strip-frontmatter "***\nauthor: Bob\n***\n\nHello")
  ;=> "Hello"

  ;; strip-frontmatter passes through content without frontmatter
  (strip-frontmatter "Just content")
  ;=> "Just content"

  ;; strip-frontmatter handles nil
  (strip-frontmatter nil)
  ;=> ""

  ;; post-card returns hiccup
  (first (post-card {:post/id 1 :post/title "Test" :post/author "Me" :post/content "Hello"} {}))
  ;=> :div.post-card

  ;; tag-list returns nil for empty
  (tag-list []) ;=> nil

  ;; tag-list returns hiccup for tags
  (first (tag-list ["a" "b"])) ;=> :div.tags

  ;; app-view dispatches on :view
  (first (app-view {:view :list :posts [] :loading? false} {}))) ;=> :div)
