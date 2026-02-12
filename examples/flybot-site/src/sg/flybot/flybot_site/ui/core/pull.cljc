(ns sg.flybot.flybot-site.ui.core.pull
  "Pull spec definitions — pattern + :then as data.

   Each named operation resolves to {:pattern ... :then ...}.
   :pattern is a pull pattern sent to the API.
   :then is either (fn [response] effect-map) or a static effect-map.

   Pure data functions — no JS dependencies, testable on JVM via RCT."
  (:require [sg.flybot.flybot-site.ui.core.db :as db]
            [sg.flybot.flybot-site.ui.core.log :as log]))

;;=============================================================================
;; Static Specs
;;=============================================================================

(def ^:private init-spec
  "Fetch posts and current user in a single pull request.
   Multi-role pattern: :guest always present, :member nil when not logged in —
   pattern matching skips nil branches naturally."
  {:pattern '{:guest {:posts ?posts} :member {:me ?user}}
   :then    (fn [r] {:db #(db/init-fetched % r)})})

(def ^:private refresh-users-spec
  "Re-fetch users list (owner only)."
  {:pattern '{:owner {:users ?users}}
   :then    (fn [r] {:db #(assoc % :admin-users (get r 'users))})})

;;=============================================================================
;; Resolve
;;=============================================================================

(defn resolve-pull
  "Resolve a named pull operation to {:pattern ... :then ...}.
   Returns nil if no fetch needed (e.g., guest with cached post).
   Mutation specs use the response directly — no re-fetch."
  [op db]
  (let [[op-key & args] (if (vector? op) op [op])]
    (case op-key
      :init init-spec

      :select-post
      (let [id         (:selected-id db)
            cached?    (some #(= (:post/id %) id) (:posts db))
            logged-in? (db/logged-in? db)]
        (cond
          (and cached? (not logged-in?)) nil
          cached?
          {:pattern {:member {:posts/history {{:post/id id} '?versions}}}
           :then    (fn [r] {:db #(assoc % :history (or (get r 'versions) []))})}
          :else
          {:pattern (cond-> {:guest {:posts {{:post/id id} '?post}}}
                      logged-in? (assoc :member {:posts/history {{:post/id id} '?versions}}))
           :then    (fn [r] {:db #(db/post-detail-fetched % r)})}))

      :create-post
      {:pattern {:member {:posts {nil (db/form->post-data db)}}}
       :then    (fn [r] {:db #(db/post-created % (get r 'posts))
                         :history :push
                         :toast {:type :success :title "Post saved"}})}

      :update-post
      (let [id       (:selected-id db)
            role-key (if (db/admin-or-owner? db) :admin :member)]
        {:pattern {role-key {:posts {{:post/id id} (db/form->post-data db)}}}
         :then    (fn [r] {:db #(db/post-updated % (get r 'posts))
                           :history :push
                           :toast {:type :success :title "Post saved"}})})

      :delete-post
      (let [id       (first args)
            role-key (if (db/admin-or-owner? db) :admin :member)]
        {:pattern {role-key {:posts {{:post/id id} nil}}}
         :then    (fn [_] {:db #(db/post-deleted % id)
                           :history :push
                           :toast {:type :success :title "Post deleted"}})})

      :restore-version
      (let [version  (first args)
            id       (:post/id version)
            role-key (if (db/admin-or-owner? db) :admin :member)]
        {:pattern {role-key {:posts {{:post/id id} {:post/title   (:post/title version)
                                                    :post/content (:post/content version)}}}}
         :then    (fn [r] {:db #(db/post-updated % (get r 'posts))
                           :history :push
                           :toast {:type :success :title "Post saved"}})})

      :fetch-profile
      {:pattern '{:member {:me/profile ?profile} :owner {:users ?users}}
       :then    (fn [r] {:db #(db/profile-fetched % r)})}

      :grant-admin
      (let [user-id (first args)]
        {:pattern {:owner {:users/roles {nil {:user/id user-id :role/name :admin}}}}
         :then    {:db #(assoc % :loading? false) :pull refresh-users-spec
                   :toast {:type :success :title "Role updated"}}})

      :revoke-admin
      (let [user-id (first args)]
        {:pattern {:owner {:users/roles {{:user/id user-id :role/name :admin} nil}}}
         :then    {:db #(assoc % :loading? false) :pull refresh-users-spec
                   :toast {:type :success :title "Role updated"}}})

      :refresh-users refresh-users-spec

      (do (log/warn "Unknown pull operation:" op-key) nil))))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; --- init ---

  (:pattern (resolve-pull :init {}))
  ;=> {:guest {:posts ?posts} :member {:me ?user}}

  ;; :then merges posts and user into db
  (let [effect ((:then (resolve-pull :init {}))
                {'posts [{:post/id 1}] 'user {:email "t@t.com"}})
        db ((:db effect) db/initial-state)]
    [(:loading? db) (count (:posts db)) (:email (:user db))])
  ;=> [false 1 "t@t.com"]

  ;; guest (no user in response)
  (let [effect ((:then (resolve-pull :init {})) {'posts [{:post/id 1}]})
        db ((:db effect) db/initial-state)]
    (:user db))
  ;=> nil

  ;; --- select-post ---

  ;; cached + guest → nil (no fetch needed)
  (resolve-pull :select-post {:selected-id 1 :posts [{:post/id 1}] :user nil})
  ;=> nil

  ;; cached + logged in → fetch history only
  (:pattern (resolve-pull :select-post {:selected-id 1
                                        :posts [{:post/id 1}]
                                        :user {:roles #{:member}}}))
  ;=> {:member {:posts/history {{:post/id 1} ?versions}}}

  ;; not cached + guest → fetch post only
  (:pattern (resolve-pull :select-post {:selected-id 42 :posts [] :user nil}))
  ;=> {:guest {:posts {{:post/id 42} ?post}}}

  ;; not cached + logged in → fetch post + history
  (let [p (:pattern (resolve-pull :select-post {:selected-id 42
                                                :posts []
                                                :user {:roles #{:member}}}))]
    [(some? (:guest p)) (some? (:member p))])
  ;=> [true true]

  ;; --- create-post ---

  ;; pattern includes form data under :member
  (let [db (assoc db/initial-state
                  :form {:title "Hi" :content "x" :tags "" :pages #{} :featured? false})
        spec (resolve-pull :create-post db)]
    (get-in spec [:pattern :member :posts nil :post/title]))
  ;=> "Hi"

  ;; :then returns history push + toast
  (let [spec (resolve-pull :create-post (assoc db/initial-state
                                               :form {:title "" :content "" :tags "" :pages #{} :featured? false}))
        effect ((:then spec) {'posts {:post/id 42}})]
    [(:history effect) (get-in effect [:toast :type])])
  ;=> [:push :success]

  ;; --- update-post ---

  ;; member user → :member role key
  (let [db {:selected-id 3 :user {:roles #{:member}}
            :form {:title "X" :content "Y" :tags "" :pages #{} :featured? false}}]
    (set (keys (:pattern (resolve-pull :update-post db)))))
  ;=> #{:member}

  ;; admin user → :admin role key
  (let [db {:selected-id 3 :user {:roles #{:admin}}
            :form {:title "X" :content "Y" :tags "" :pages #{} :featured? false}}]
    (set (keys (:pattern (resolve-pull :update-post db)))))
  ;=> #{:admin}

  ;; --- delete-post ---

  ;; member → :member role key
  (:pattern (resolve-pull [:delete-post 5] {:user {:roles #{:member}}}))
  ;=> {:member {:posts {{:post/id 5} nil}}}

  ;; admin → :admin role key
  (:pattern (resolve-pull [:delete-post 5] {:user {:roles #{:admin}}}))
  ;=> {:admin {:posts {{:post/id 5} nil}}}

  ;; :then removes post from list
  (let [spec (resolve-pull [:delete-post 1] {:user {:roles #{:member}}})
        effect ((:then spec) {'posts true})
        db ((:db effect) {:posts [{:post/id 1} {:post/id 2}] :loading? true :view :detail})]
    [(count (:posts db)) (:view db)])
  ;=> [1 :list]

  ;; --- restore-version ---

  (let [version {:post/id 7 :post/title "Old Title" :post/content "Old Content"}
        spec (resolve-pull [:restore-version version] {:user {:roles #{:member}}})]
    (get-in spec [:pattern :member :posts {:post/id 7} :post/title]))
  ;=> "Old Title"

  ;; --- fetch-profile ---

  (:pattern (resolve-pull :fetch-profile {}))
  ;=> {:member {:me/profile ?profile} :owner {:users ?users}}

  ;; --- grant/revoke admin ---

  (get-in (:pattern (resolve-pull [:grant-admin "u1"] {}))
          [:owner :users/roles nil :role/name])
  ;=> :admin

  (:pattern (resolve-pull [:revoke-admin "u1"] {}))
  ;=> {:owner {:users/roles {{:user/id "u1" :role/name :admin} nil}}}

  ;; --- unknown operation ---

  (resolve-pull :nonexistent {}))
  ;=> nil)
