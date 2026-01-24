(ns sg.flybot.blog.ui.core
  "Blog SPA entry point - browser-only wiring."
  (:require [replicant.dom :as r]
            [sg.flybot.blog.ui.state :as state]
            [sg.flybot.blog.ui.views :as views]
            [sg.flybot.blog.ui.api :as api]
            [sg.flybot.blog.ui.log :as log]))

;;=============================================================================
;; App State (mutable)
;;=============================================================================

(defonce app-state (atom state/initial-state))

(defn swap-state! [f & args]
  (let [old @app-state
        new (apply swap! app-state f args)]
    (when (not= (:view old) (:view new))
      (log/log-state-change (str f) old new))
    new))

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

(defn load-history! [post-id]
  (swap-state! state/set-loading true)
  (api/pull!
   {:posts/history {{:post/id post-id} '?versions}}
   (fn [response]
     (let [versions (get response 'versions)]
       (swap-state! state/set-history versions)
       (swap-state! state/set-view :history post-id)))
   (fn [err]
     (swap-state! state/set-error err))))

(defn restore-version! [version]
  (when (js/confirm "Restore this version? The current content will be overwritten.")
    (let [id (:post/id version)
          data {:post/title (:post/title version)
                :post/content (:post/content version)}]
      (swap-state! state/set-loading true)
      (api/pull!
       {:posts {{:post/id id} data}}
       (fn [_]
         (swap-state! state/set-view :detail id)
         (load-posts!))
       (fn [err]
         (swap-state! state/set-error err))))))

;;=============================================================================
;; Event Handlers
;;=============================================================================

(defn- get-input-value [e]
  (.. e -target -value))

(defn- load-history-for-post! [post-id]
  "Load history for a post (for badge display). Doesn't show loading spinner."
  (api/pull!
   {:posts/history {{:post/id post-id} '?versions}}
   (fn [response]
     (swap-state! state/set-history (get response 'versions)))
   (fn [_] nil)))  ; Silently ignore errors

(def actions
  "Action handlers passed to views."
  {:on-select (fn [id]
                (swap-state! state/set-view :detail id)
                (load-history-for-post! id))
   :on-back (fn [e target]
              (.preventDefault e)
              (swap-state! state/set-view target (:selected-id @app-state)))
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
                  (create-post!)))
   :on-history load-history!
   :on-view-version (fn [version]
                      (swap-state! state/set-history-version version)
                      (swap-state! state/set-view :history-detail))
   :on-restore restore-version!})

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
  (log/info "Blog app initializing...")
  (reset! root-el (js/document.getElementById "app"))
  (load-posts!)
  (render!)
  (log/info "Blog app initialized"))

;; Initialize on load
(init!)
