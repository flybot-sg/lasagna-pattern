(ns sg.flybot.blog.ui.state
  "Application state - pure data manipulation."
  (:require [clojure.string :as str]
            [sg.flybot.blog.markdown :as md]))

;;=============================================================================
;; State Shape
;;=============================================================================

(def initial-state
  {:view :list        ; :list | :detail | :edit | :new
   :posts []
   :selected-id nil
   :loading? false
   :error nil
   :form {:title "" :content "" :author "" :tags ""}})

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
  (assoc state :view view :selected-id post-id))

(defn update-form [state field value]
  (assoc-in state [:form field] value))

(defn reset-form [state]
  (assoc state :form {:title "" :content "" :author "" :tags ""}))

(defn set-form-from-post [state post]
  (let [parsed (md/parse (:post/content post))]
    (assoc state :form
           {:title (:post/title post "")
            :content (:content parsed "")
            :author (:author parsed (:post/author post ""))
            :tags (str/join ", " (:tags parsed (:post/tags post [])))})))

;;=============================================================================
;; Selectors (pure functions)
;;=============================================================================

(defn selected-post [state]
  (let [{:keys [posts selected-id]} state]
    (first (filter #(= (:post/id %) selected-id) posts))))

(defn parse-tags [tags-str]
  (->> (str/split tags-str #",")
       (map str/trim)
       (remove empty?)
       vec))

(defn build-content
  "Build markdown content with YAML frontmatter from form fields."
  [{:keys [content author tags]}]
  (let [tag-list (parse-tags tags)
        has-frontmatter? (and (seq author) (or (seq author) (seq tag-list)))]
    (if has-frontmatter?
      (str "---\n"
           (when (seq author) (str "author: " author "\n"))
           (when (seq tag-list)
             (str "tags:\n" (str/join "" (map #(str "  - " % "\n") tag-list))))
           "---\n\n"
           content)
      content)))

(defn form->post-data [state]
  (let [{:keys [title] :as form} (:form state)]
    {:post/title title
     :post/content (build-content form)}))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  (set-loading initial-state true)
  ;=> {:view :list, :posts [], :selected-id nil, :loading? true, :error nil, :form {:title "", :content "", :author "", :tags ""}}

  (-> initial-state
      (set-posts [{:id 1 :title "Test"}])
      :posts
      count)
  ;=> 1

  (parse-tags "clojure, web, api")
  ;=> ["clojure" "web" "api"]

  (parse-tags "")
  ;=> []

  ;; build-content creates frontmatter
  (build-content {:content "Hello" :author "Me" :tags "a, b"})
  ;=> "---\nauthor: Me\ntags:\n  - a\n  - b\n---\n\nHello"

  ;; form->post-data builds content with frontmatter
  (-> initial-state
      (assoc :form {:title "T" :content "C" :author "A" :tags "x, y"})
      form->post-data
      :title))
  ;=> "T")
