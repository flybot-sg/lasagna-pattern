(ns sg.flybot.blog.ui.core
  "Blog SPA entry point - browser-only wiring."
  (:require [replicant.dom :as r]
            [sg.flybot.blog.ui.state :as state]
            [sg.flybot.blog.ui.views :as views]
            [sg.flybot.blog.ui.api :as api]))

;;=============================================================================
;; App State (mutable)
;;=============================================================================

(defonce app-state (atom state/initial-state))

(defn swap-state! [f & args]
  (apply swap! app-state f args))

;;=============================================================================
;; API Actions
;;=============================================================================

(defn load-posts! []
  (swap-state! state/set-loading true)
  (api/pull!
   '{:posts ?posts}
   (fn [response]
     ;; Response is {posts [...]} (var name without ?)
     (let [posts (get response 'posts)]
       (swap-state! state/set-posts posts)))
   (fn [err]
     (swap-state! state/set-error err))))

(defn create-post! []
  (let [data (state/form->post-data @app-state)]
    (swap-state! state/set-loading true)
    (api/pull!
     {:posts {nil data}}
     (fn [_]
       (swap-state! state/reset-form)
       (swap-state! state/set-view :list)
       (load-posts!))
     (fn [err]
       (swap-state! state/set-error err)))))

(defn update-post! []
  (let [id (:selected-id @app-state)
        data (state/form->post-data @app-state)]
    (swap-state! state/set-loading true)
    (api/pull!
     {:posts {{:post/id id} data}}
     (fn [_]
       (swap-state! state/set-view :list)
       (load-posts!))
     (fn [err]
       (swap-state! state/set-error err)))))

(defn delete-post! [id]
  (when (js/confirm "Delete this post?")
    (swap-state! state/set-loading true)
    (api/pull!
     {:posts {{:post/id id} nil}}
     (fn [_]
       (swap-state! state/set-view :list)
       (load-posts!))
     (fn [err]
       (swap-state! state/set-error err)))))

;;=============================================================================
;; Event Handlers
;;=============================================================================

(defn- get-input-value [e]
  (.. e -target -value))

(def actions
  "Action handlers passed to views."
  {:on-select (fn [id] (swap-state! state/set-view :detail id))
   :on-back (fn [e]
              (.preventDefault e)
              (swap-state! state/set-view :list))
   :on-new (fn [_]
             (swap-state! state/reset-form)
             (swap-state! state/set-view :new))
   :on-edit (fn [post]
              (swap-state! state/set-form-from-post post)
              (swap-state! state/set-view :edit (:post/id post)))
   :on-delete delete-post!
   :on-field (fn [field e]
               (swap-state! state/update-form field (get-input-value e)))
   :on-content (fn [markdown]
                 (swap-state! state/update-form :content markdown))
   :on-submit (fn [_]
                (if (= :edit (:view @app-state))
                  (update-post!)
                  (create-post!)))})

;;=============================================================================
;; Rendering
;;=============================================================================

(defonce root-el (atom nil))

(defn render! []
  (when @root-el
    (r/render @root-el (views/app-view @app-state actions))))

;; Re-render on state change
(add-watch app-state :render (fn [_ _ _ _] (render!)))

(defn ^:export init! []
  (reset! root-el (js/document.getElementById "app"))
  (load-posts!)
  (render!))

;; Initialize on load
(init!)
