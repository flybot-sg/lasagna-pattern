(ns sg.flybot.blog.ui.state
  "Application state - pure data manipulation."
  (:require [clojure.string :as str]))

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
  (assoc state :form
         {:title (:title post "")
          :content (:content post "")
          :author (:author post "")
          :tags (str/join ", " (:tags post []))}))

;;=============================================================================
;; Selectors (pure functions)
;;=============================================================================

(defn selected-post [state]
  (let [{:keys [posts selected-id]} state]
    (first (filter #(= (:id %) selected-id) posts))))

(defn parse-tags [tags-str]
  (->> (str/split tags-str #",")
       (map str/trim)
       (remove empty?)
       vec))

(defn form->post-data [state]
  (let [{:keys [title content author tags]} (:form state)]
    {:title title
     :content content
     :author author
     :tags (parse-tags tags)}))

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

  (-> initial-state
      (assoc :form {:title "T" :content "C" :author "A" :tags "x, y"})
      form->post-data))
  ;=> {:title "T", :content "C", :author "A", :tags ["x" "y"]})
