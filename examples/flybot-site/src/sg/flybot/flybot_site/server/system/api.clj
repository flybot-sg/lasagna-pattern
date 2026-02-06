(ns sg.flybot.flybot-site.server.system.api
  "Blog API - noun-only schema with role-based authorization.

   ## Design

   The schema is purely nouns - no verbs like create/update/delete.
   CRUD operations are expressed through pattern syntax on collections.
   Authorization is role-as-top-level: each role key contains its accessible resources.

   ## Data Structure

   | Role Key | Access   | Contents                                    |
   |----------|----------|---------------------------------------------|
   | :guest   | public   | {:posts read-only, :posts/history}          |
   | :member  | :member  | {:posts CRUD-own, :posts/history, :me}      |
   | :admin   | :admin   | {:posts CRUD-any}                           |
   | :owner   | :owner   | {:users collection}                         |

   ## Pattern-Based CRUD (role in pattern)

   ```clojure
   '{:guest {:posts ?all}}                       ; guest list
   '{:guest {:posts/history {{:post/id 1} ?v}}}  ; guest history
   {:member {:posts {nil {:post/title ...}}}}    ; member create
   {:member {:posts {{:post/id 1} nil}}}         ; member delete own
   {:admin {:posts {{:post/id 1} {...}}}}        ; admin update any
   '{:owner {:users ?all}}                       ; owner list users
   ```

   See remote/doc/SPECIFICATION.md for wire protocol."
  (:require
   [sg.flybot.flybot-site.server.system.api.role :as role]
   [sg.flybot.flybot-site.server.system.db :as db]
   [sg.flybot.pullable.collection :as coll]
   [sg.flybot.pullable.malli]
   [sg.flybot.pullable.sample :as sample]
   [malli.core :as m]
   [malli.util :as mu]))

;;=============================================================================
;; Schema (for remote validation)
;;=============================================================================

(def user-schema
  "Schema for a user."
  (m/schema
   [:map {:doc "User account"}
    [:user/id {:doc "Google user ID"} :string]
    [:user/email {:doc "User email"} :string]
    [:user/name {:doc "Display name"} :string]
    [:user/slug {:doc "URL-safe identifier"} :string]
    [:user/picture {:doc "Profile picture URL"} :string]]))

(def post-schema
  "Schema for a single post."
  (m/schema
   [:map {:doc "Blog post"}
    [:post/id {:doc "Unique identifier"} :int]
    [:post/title {:doc "Post title"} :string]
    [:post/content {:doc "Markdown content"} :string]
    [:post/author {:doc "Author (user reference)" :optional true} [:maybe user-schema]]
    [:post/tags {:doc "List of tags"} [:vector :string]]
    [:post/featured? {:doc "Featured post" :optional true} :boolean]
    [:post/created-at {:doc "Creation timestamp"} :any]
    [:post/updated-at {:doc "Last update timestamp"} :any]]))

(def post-query
  "Query schema for post lookup (indexed fields)."
  (m/schema
   [:or
    [:map [:post/id :int]]
    [:map [:post/author :string]]]))

(def version-schema
  "Schema for a historical version of a post."
  (mu/merge
   post-schema
   (m/schema
    [:map {:doc "Historical version"}
     [:version/tx {:doc "Transaction ID"} :int]
     [:version/timestamp {:doc "When this version was created"} :any]])))

(def ^:private posts-schema
  "Schema for posts collection (list or lookup)."
  (m/schema [:or [:vector post-schema] [:map-of post-query post-schema]]))

(def ^:private history-schema
  "Schema for post history lookup."
  (m/schema [:map-of post-query [:vector version-schema]]))

(def ^:private me-schema
  "Schema for current user info."
  (m/schema [:map
             [:id :string]
             [:email :string]
             [:name :string]
             [:picture :string]
             [:slug {:optional true} [:maybe :string]]
             [:roles [:set :keyword]]]))

(def ^:private role-detail-schema
  "Schema for a role with grant date."
  (m/schema [:map
             [:role/name :keyword]
             [:role/granted-at :any]]))

(def ^:private profile-schema
  "Schema for user profile data."
  (m/schema [:map
             [:post-count :int]
             [:revision-count :int]
             [:roles [:vector role-detail-schema]]]))

(def schema
  "Public API schema (used by remote for validation).
   Role-as-top-level structure - each role key contains its accessible resources."
  {:guest (m/schema [:map
                     [:posts posts-schema]])
   :member (m/schema [:maybe [:map
                              [:posts posts-schema]
                              [:posts/history history-schema]
                              [:me me-schema]
                              [:me/profile profile-schema]]])
   :admin (m/schema [:maybe [:map
                             [:posts posts-schema]]])
   :owner (m/schema [:maybe [:map
                             [:users [:vector user-schema]]
                             [:users/roles [:map-of
                                            [:map [:user/id :string]]
                                            [:vector [:map [:role/name :keyword]]]]]]])})

(def ^:private sample-data
  "Sample data for GET /api/_schema introspection."
  (sample/generate
   [:map
    [:guest [:map
             [:posts [:vector post-schema]]]]
    [:member [:map
              [:posts [:vector post-schema]]
              [:me me-schema]]]]
   {:size 10 :seed 42 :min 5}))

^:rct/test
(comment
  (m/schema? post-schema) ;=> true
  (m/schema? post-query) ;=> true
  (m/schema? version-schema) ;=> true
  (m/schema? (:guest schema)) ;=> true
  (m/schema? (:member schema))) ;=> true

;;=============================================================================
;; Collection Wrappers
;;=============================================================================

(defn- post-author-id [post]
  (get-in post [:post/author :user/id]))

(defn- owns-post? [posts user-id query]
  (when-let [post (get posts query)]
    (= (post-author-id post) user-id)))

(deftype MemberPosts [posts user-id]
  ;; Collection wrapper that enforces ownership on mutations.
  ;; - CREATE: Sets :post/author to user-id
  ;; - UPDATE: Only allowed if user owns the post
  ;; - DELETE: Only allowed if user owns the post
  clojure.lang.ILookup
  (valAt [_ query] (get posts query))
  (valAt [_ query not-found] (get posts query not-found))

  clojure.lang.Seqable
  (seq [_] (seq posts))

  clojure.lang.Counted
  (count [_] (count posts))

  coll/Mutable
  (mutate! [_ query value]
    (cond
      ;; CREATE: nil query, some value -> set author
      (and (nil? query) (some? value))
      (coll/mutate! posts nil (assoc value :post/author user-id))

      ;; DELETE: some query, nil value -> check ownership
      (and (some? query) (nil? value))
      (if (owns-post? posts user-id query)
        (coll/mutate! posts query nil)
        {:error {:type :forbidden :message "You don't own this post"}})

      ;; UPDATE: some query, some value -> check ownership
      (and (some? query) (some? value))
      (if (owns-post? posts user-id query)
        (coll/mutate! posts query value)
        {:error {:type :forbidden :message "You don't own this post"}})

      :else
      {:error {:type :invalid-mutation :message "Invalid mutation operation"}}))

  coll/Wireable
  (->wire [_] (coll/->wire posts)))

(defn- member-posts
  "Create a posts collection with ownership enforcement."
  [posts user-id]
  (->MemberPosts posts user-id))

;;=============================================================================
;; Users Collection (Owner only)
;;=============================================================================

(deftype UsersCollection [conn]
  ;; Collection for user management (owner only).
  ;; Supports listing users. Role grant/revoke via nested :roles collection.
  clojure.lang.ILookup
  (valAt [_ query] (db/get-user conn (:user/id query)))
  (valAt [this query not-found] (or (.valAt this query) not-found))

  clojure.lang.Seqable
  (seq [_] (seq (db/list-users conn)))

  clojure.lang.Counted
  (count [_] (count (db/list-users conn)))

  coll/Wireable
  (->wire [this] (vec (seq this))))

(defn- users-collection [conn]
  (->UsersCollection conn))

;;-----------------------------------------------------------------------------

(deftype RolesCollection [conn user-id]
  ;; Collection for managing roles of a specific user.
  ;; - CREATE (nil query): Grant role
  ;; - DELETE (query, nil value): Revoke role
  clojure.lang.ILookup
  (valAt [_ query]
    (let [roles (db/get-user-roles conn user-id)]
      (when (contains? roles (:role/name query))
        {:role/name (:role/name query)})))
  (valAt [this query not-found] (or (.valAt this query) not-found))

  clojure.lang.Seqable
  (seq [_]
    (map (fn [r] {:role/name r}) (db/get-user-roles conn user-id)))

  clojure.lang.Counted
  (count [_] (count (db/get-user-roles conn user-id)))

  coll/Mutable
  (mutate! [_ query value]
    (cond
      ;; GRANT: nil query, value with :role/name
      (and (nil? query) (some? value))
      (let [role (:role/name value)
            existing (db/get-user-roles conn user-id)]
        (if (contains? existing role)
          {:error {:type :already-granted :message (str "Role already granted: " (name role))}}
          (do (db/grant-role! conn user-id role)
              {:role/name role :granted true})))

      ;; REVOKE: query with :role/name, nil value
      (and (some? query) (nil? value))
      (let [role (:role/name query)
            existing (db/get-user-roles conn user-id)]
        (if (contains? existing role)
          (do (db/revoke-role! conn user-id role)
              {:role/name role :revoked true})
          {:error {:type :not-found :message (str "Role not found: " (name role))}}))

      :else
      {:error {:type :invalid-mutation :message "Invalid mutation operation"}}))

  coll/Wireable
  (->wire [this] (vec (seq this))))

(defn- roles-collection [conn user-id]
  (->RolesCollection conn user-id))

(defn- roles-lookup
  "Mutable ILookup for role management.
   - GET: (get lookup {:user/id \"..\"}) => list of roles for user
   - CREATE (grant): (mutate! lookup nil {:user/id \"..\" :role/name :admin})
   - DELETE (revoke): (mutate! lookup {:user/id \"..\" :role/name :admin} nil)"
  [conn]
  (reify
    clojure.lang.ILookup
    (valAt [_ query]
      (when-let [uid (:user/id query)]
        (vec (seq (roles-collection conn uid)))))
    (valAt [this query not-found]
      (or (.valAt this query) not-found))

    coll/Mutable
    (mutate! [_ query value]
      (cond
        ;; GRANT: nil query, value with :user/id and :role/name
        (and (nil? query) (some? value))
        (let [{:keys [user/id role/name]} value]
          (if (and id name)
            (do (db/grant-role! conn id name)
                {:user/id id :role/name name :granted true})
            {:error {:type :invalid-mutation :message "Requires :user/id and :role/name"}}))

        ;; REVOKE: query with :user/id and :role/name, nil value
        (and (some? query) (nil? value))
        (let [{:keys [user/id role/name]} query]
          (if (and id name)
            (do (db/revoke-role! conn id name)
                {:user/id id :role/name name :revoked true})
            {:error {:type :invalid-mutation :message "Requires :user/id and :role/name"}}))

        :else
        {:error {:type :invalid-mutation :message "Invalid role operation"}}))

    coll/Wireable
    (->wire [_] nil)))

;;=============================================================================
;; API Builder
;;=============================================================================

(defn- me-lookup
  "Lazy ILookup for current user info.
   Most fields come from the session (free), but :slug requires a DB lookup
   which only runs when the pattern accesses it."
  [conn session]
  (let [user-id (:user-id session)]
    (reify
      clojure.lang.ILookup
      (valAt [this k] (.valAt this k nil))
      (valAt [_ k not-found]
        (case k
          :id user-id
          :email (:user-email session)
          :name (or (:user-name session) (:user-email session))
          :picture (:user-picture session)
          :slug (:user/slug (db/get-user conn user-id))
          :roles (or (:roles session) #{})
          not-found))

      coll/Wireable
      (->wire [_]
        {:id user-id
         :email (:user-email session)
         :name (or (:user-name session) (:user-email session))
         :picture (:user-picture session)
         :slug (:user/slug (db/get-user conn user-id))
         :roles (or (:roles session) #{})}))))

(defn- profile-lookup
  "Lazy ILookup for user profile data.
   Cheap to create (just a closure) — DB queries only run when ->wire is called,
   i.e. when the pattern matcher binds ?profile."
  [conn user-id]
  (reify
    clojure.lang.ILookup
    (valAt [this k] (.valAt this k nil))
    (valAt [_ k not-found]
      (case k
        :post-count (db/count-user-posts conn user-id)
        :revision-count (db/count-user-revisions conn user-id)
        :roles (db/get-user-roles-detailed conn user-id)
        not-found))

    coll/Wireable
    (->wire [_]
      {:post-count (db/count-user-posts conn user-id)
       :revision-count (db/count-user-revisions conn user-id)
       :roles (db/get-user-roles-detailed conn user-id)})))

(def ^:private error-config
  "Error handling config for remote layer.
   - :detect - how to detect errors in mutation results
   - :codes  - map error types to HTTP status codes"
  {:detect :error
   :codes {:forbidden 403
           :not-found 404
           :invalid-mutation 422
           :already-granted 422}})

(defn make-api
  "Create API function for wrap-api middleware.

   Returns (fn [ring-request] {:data ... :schema ... :errors ... :sample ...})

   Data structure (role as top-level, nil if session lacks role):
   - :guest  - always available: {:posts}
   - :member - requires :member: {:posts, :posts/history, :me, :me/profile}
   - :admin  - requires :admin: {:posts}
   - :owner  - requires :owner: {:users, :users/roles}

   :me and :me/profile are lazy ILookups — DB queries only run when the
   pattern matcher accesses their fields (e.g. :slug triggers db/get-user)."
  [{:keys [conn]}]
  (fn [ring-request]
    (let [session (:session ring-request)
          user-id (:user-id session)
          posts (db/posts conn)
          history (db/post-history-lookup conn)]

      {:data
       {;; Guest: always available (read-only posts only)
        :guest {:posts (coll/read-only posts)}

        ;; Member: values are lazy ILookups — no DB calls until pattern accesses them
        :member (role/with-role session :member
                  {:posts (member-posts posts user-id)
                   :posts/history history
                   :me (me-lookup conn session)
                   :me/profile (profile-lookup conn user-id)})

        ;; Admin: CRUD any post
        :admin (role/with-role session :admin
                 {:posts posts})

        ;; Owner: user management + role grant/revoke
        :owner (role/with-role session :owner
                 {:users (users-collection conn)
                  :users/roles (roles-lookup conn)})}

       :schema schema
       :errors error-config
       :sample sample-data})))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; Setup
  (def conn (db/create-conn!))
  (db/seed! conn)
  (def api-fn (make-api {:conn conn}))

  ;; Guest: can read posts via :guest
  (count (seq (get-in (api-fn {}) [:data :guest :posts]))) ;=> 11
  (:post/title (get (get-in (api-fn {}) [:data :guest :posts]) {:post/id 1})) ;=> "Welcome to Flybot"

  ;; Guest: :guest always present, other roles nil
  (let [{:keys [data]} (api-fn {})]
    [(some? (:guest data))
     (nil? (:member data))
     (nil? (:admin data))
     (nil? (:owner data))])
  ;=> [true true true true]

  ;; Member: has :member with :posts, :posts/history, :me
  (db/create-user! conn #:user{:id "m1" :email "m@test.com" :name "M" :picture ""})
  (db/grant-role! conn "m1" :member)
  (let [session {:user-id "m1" :user-email "m@test.com" :roles #{:member}}
        {:keys [data]} (api-fn {:session session})]
    [(some? (:member data))
     (some? (get-in data [:member :posts]))
     (some? (get-in data [:member :me]))
     (get-in data [:member :me :email])
     (nil? (:admin data))])
  ;=> [true true true "m@test.com" true]

  ;; Member: can create post via :member :posts
  (let [session {:user-id "m1" :user-email "m@test.com" :roles #{:member}}
        {:keys [data]} (api-fn {:session session})
        result (coll/mutate! (get-in data [:member :posts]) nil
                             {:post/title "My Post" :post/content "x" :post/tags []})]
    (:post/title result))
  ;=> "My Post"

  ;; Member: cannot update other's post
  (let [session {:user-id "m1" :user-email "m@test.com" :roles #{:member}}
        {:keys [data]} (api-fn {:session session})
        result (coll/mutate! (get-in data [:member :posts]) {:post/id 1} {:post/title "Hacked"})]
    (:type (:error result)))
  ;=> :forbidden

  ;; Admin: can update any post via :admin :posts
  (db/create-user! conn #:user{:id "a1" :email "a@test.com" :name "A" :picture ""})
  (db/grant-role! conn "a1" :admin)
  (let [session {:user-id "a1" :user-email "a@test.com" :roles #{:admin}}
        {:keys [data]} (api-fn {:session session})
        result (coll/mutate! (get-in data [:admin :posts]) {:post/id 1} {:post/title "Admin Edit"})]
    (:post/title result))
  ;=> "Admin Edit"

  ;; Owner: has :owner with :users
  (db/create-user! conn #:user{:id "o1" :email "o@test.com" :name "O" :picture ""})
  (db/grant-role! conn "o1" :owner)
  (let [session {:user-id "o1" :user-email "o@test.com" :roles #{:owner}}
        {:keys [data]} (api-fn {:session session})]
    (some? (get-in data [:owner :users])))
  ;=> true

  (db/release-conn! conn))
