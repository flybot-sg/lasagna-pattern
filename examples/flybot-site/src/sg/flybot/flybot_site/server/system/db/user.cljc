(ns sg.flybot.flybot-site.server.system.db.user
  "User CRUD operations and UsersDataSource.

   Handles user creation, lookup, slug generation, and placeholder claiming.
   Slugs are auto-generated from names (supports Chinese via pinyin)."
  (:require
   [datahike.api :as d]
   [sg.flybot.pullable.collection :as coll]
   [com.brunobonacci.mulog :as mu]
   [clojure.string :as str]
   [pinyin4clj.core :as pinyin]))

;;=============================================================================
;; Entity Conversion
;;=============================================================================

(def ^:private user-attr-keys
  "User attribute keys (namespaced)."
  [:user/id :user/email :user/name :user/slug :user/picture])

(defn- normalize-user
  "Normalize Datahike user entity to consistent map (no roles)."
  [entity]
  (when entity
    (select-keys entity user-attr-keys)))

(defn- normalize-user-with-roles
  "Normalize Datahike user entity with roles as keyword set.
   Used by UsersDataSource to return complete user data."
  [entity]
  (when entity
    (-> (select-keys entity user-attr-keys)
        (assoc :user/roles (set (map :role/name (:user/roles entity)))))))

;;=============================================================================
;; Slug Generation
;;=============================================================================

(defn slugify
  "Convert a name to URL-safe slug. Handles Chinese via pinyin.
   'Bob Smith' -> 'bob-smith'
   '张伟' -> 'zhangwei'"
  [s]
  (when s
    (-> s
        pinyin/ascii-pinyin
        str/lower-case
        (str/replace #"[^a-z0-9]+" "-")
        (str/replace #"^-|-$" ""))))

^:rct/test
(comment
  (slugify "Bob Smith") ;=> "bob-smith"
  (slugify "Bob O'Brien") ;=> "bob-o-brien"
  (slugify "张伟") ;=> "zhangwei"
  )

(defn- slug-exists?
  "Check if a slug already exists in the database."
  [conn slug]
  (some? (d/q '[:find ?e .
                :in $ ?slug
                :where [?e :user/slug ?slug]]
              @conn slug)))

(defn- generate-unique-slug
  "Generate a unique slug, appending -2, -3, etc. if needed."
  [conn base-slug]
  (if-not (slug-exists? conn base-slug)
    base-slug
    (loop [n 2]
      (let [candidate (str base-slug "-" n)]
        (if-not (slug-exists? conn candidate)
          candidate
          (recur (inc n)))))))

;;=============================================================================
;; User Queries
;;=============================================================================

(defn get-user
  "Get user by Google ID. Returns nil if not found."
  [conn user-id]
  (when user-id
    (normalize-user
     (d/q '[:find (pull ?e [*]) .
            :in $ ?id
            :where [?e :user/id ?id]]
          @conn user-id))))

(defn get-user-by-slug
  "Get user by slug. Returns nil if not found."
  [conn slug]
  (when slug
    (normalize-user
     (d/q '[:find (pull ?e [*]) .
            :in $ ?slug
            :where [?e :user/slug ?slug]]
          @conn slug))))

(defn get-user-by-name
  "Get user by exact name match. Returns nil if not found."
  [conn user-name]
  (when user-name
    (normalize-user
     (d/q '[:find (pull ?e [*]) .
            :in $ ?name
            :where [?e :user/name ?name]]
          @conn user-name))))

(defn get-placeholder-by-name
  "Get placeholder user by name. Returns nil if not a placeholder.
   Placeholder users have user/id starting with 'placeholder:'."
  [conn user-name]
  (when-let [user (get-user-by-name conn user-name)]
    (when (str/starts-with? (:user/id user) "placeholder:")
      user)))

;;=============================================================================
;; User Mutations
;;=============================================================================

(defn claim-placeholder!
  "Claim a placeholder user by updating its user-id to the real Google sub.
   Updates the entity in place, preserving posts that reference it."
  [conn placeholder-user {:user/keys [id] :as user-data}]
  (let [old-id (:user/id placeholder-user)
        eid (d/q '[:find ?e . :in $ ?id :where [?e :user/id ?id]] @conn old-id)
        slug (or (:user/slug placeholder-user) (slugify (:user/name user-data)))
        updates (-> (select-keys user-data user-attr-keys)
                    (assoc :db/id eid :user/slug slug))]
    (d/transact conn [updates])
    (mu/log ::placeholder-claimed :name (:user/name user-data) :old-id old-id :new-id id)
    (get-user conn id)))

(defn create-user!
  "Create a new user. Auto-generates slug from name. Returns the created user."
  [conn {:user/keys [name] :as user-data}]
  (mu/log ::user-create :name name)
  (let [base-slug (slugify name)
        slug (generate-unique-slug conn base-slug)
        entity (-> (select-keys user-data user-attr-keys)
                   (assoc :user/slug slug))]
    (d/transact conn [entity])
    (normalize-user entity)))

(defn upsert-user!
  "Create user if not exists, or update if exists. Returns user.
   Also claims placeholder users with matching name on first login."
  [conn {:user/keys [id name] :as user-data}]
  (if-let [existing (get-user conn id)]
    ;; Update existing user (name/email/picture may have changed, keep slug stable)
    (let [updates (-> (select-keys user-data user-attr-keys)
                      (assoc :user/slug (:user/slug existing)))]
      (mu/log ::user-update :name (:user/name user-data))
      (d/transact conn [updates])
      (get-user conn id))
    ;; Check for placeholder to claim
    (if-let [placeholder (get-placeholder-by-name conn name)]
      (claim-placeholder! conn placeholder user-data)
      ;; Create new user
      (create-user! conn user-data))))

;;=============================================================================
;; User Statistics
;;=============================================================================

(defn count-user-posts
  "Count posts authored by user via Datalog aggregate.
   Returns 0 if user has no posts."
  [conn user-id]
  (or (when user-id
        (d/q '[:find (count ?p) .
               :in $ ?uid
               :where
               [?u :user/id ?uid]
               [?p :post/author ?u]]
             @conn user-id))
      0))

(defn count-user-revisions
  "Count total revisions (edits) across all posts authored by user.
   First finds post entity IDs from current DB, then counts unique transactions
   in history DB that touched content or title, minus one per post (initial creation).
   Returns 0 if user has no edits."
  [conn user-id]
  (if-not user-id
    0
    (let [db         @conn
          ;; Step 1: get entity IDs of user's posts from current DB
          post-eids  (d/q '[:find [?p ...]
                            :in $ ?uid
                            :where
                            [?u :user/id ?uid]
                            [?p :post/author ?u]]
                          db user-id)
          ;; Step 2: count unique transactions that touched content or title
          hist       (d/history db)
          tx-count   (if (seq post-eids)
                       (or (d/q '[:find (count-distinct ?tx) .
                                  :in $ [?p ...]
                                  :where
                                  (or [?p :post/content _ ?tx true]
                                      [?p :post/title _ ?tx true])]
                                hist post-eids)
                           0)
                       0)]
      (max 0 (- tx-count (count post-eids))))))

;;=============================================================================
;; User Listing
;;=============================================================================

(defn list-users
  "List all users with their roles."
  [conn]
  (->> (d/q '[:find [(pull ?u [* {:user/roles [*]}]) ...]
              :where [?u :user/id _]]
            @conn)
       (map normalize-user-with-roles)))

;;=============================================================================
;; Collection Constructor
;;=============================================================================

(defn- fetch-user-with-roles
  "Fetch user by ID with roles expanded. Used by UsersDataSource."
  [conn user-id]
  (when user-id
    (normalize-user-with-roles
     (d/q '[:find (pull ?e [* {:user/roles [*]}]) .
            :in $ ?id
            :where [?e :user/id ?id]]
          @conn user-id))))

(defrecord UsersDataSource [conn]
  coll/DataSource
  (fetch [_ query] (fetch-user-with-roles conn (:user/id query)))
  (list-all [_] (list-users conn))
  (create! [_ data]
    (create-user! conn data)
    (fetch-user-with-roles conn (:user/id data)))
  (update! [_ _query _data] nil)
  (delete! [_ _query] false))

(defn users
  "Create a users collection for the given Datahike connection.
   Read-only by convention — user creation happens via OAuth login, not the API."
  [conn]
  (coll/collection (->UsersDataSource conn)
                   {:id-key :user/id :indexes #{#{:user/id}}}))
