(ns sg.flybot.flybot-site.db
  "Datahike-backed blog database.

   Implements DataSource for CRUD operations using Datahike.
   Use `posts` to get a collection for pattern-based access.

   ## Markdown Format

   Posts are stored as markdown with YAML frontmatter:

   ```markdown
   ---
   author: Alice
   tags:
     - clojure
     - patterns
   ---

   The actual post content in markdown...
   ```

   ## Usage

   ```clojure
   (def conn (create-conn!))
   (def p (posts conn))
   (seq p)                        ; list all
   (get p {:id 3})                ; fetch by id
   (coll/mutate! p nil data)      ; create
   (coll/mutate! p {:id 3} data)  ; update
   (coll/mutate! p {:id 3} nil)   ; delete
   ```"
  (:require
   [datahike.api :as d]
   [datahike-s3.core] ; registers :s3 backend
   [sg.flybot.pullable.collection :as coll]
   [sg.flybot.flybot-site.markdown :as md]
   [com.brunobonacci.mulog :as mu]
   [clojure.string :as str]
   [pinyin4clj.core :as pinyin]))

;;=============================================================================
;; Schema
;;=============================================================================

(def user-schema
  "Datahike schema for users.

   Users are created on first OAuth login. The :user/id is the Google 'id' (sub claim),
   which is stable across email/name changes. The :user/slug is a URL-safe identifier
   derived from the user's name (e.g., 'bob-smith')."
  [{:db/ident :user/id
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :user/email
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :user/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :user/slug
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :user/picture
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(def post-schema
  "Datahike schema for posts.

   Content is stored as markdown with YAML frontmatter containing author/tags.
   Author is a reference to user entity."
  [{:db/ident :post/id
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :post/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :post/content
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :post/author
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :post/tags
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}
   {:db/ident :post/created-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :post/updated-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :post/featured?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one}])

(def db-schema
  "Combined schema for all entities."
  (concat user-schema post-schema))

(def ^:private user-attr-keys
  "User attribute keys (namespaced)."
  [:user/id :user/email :user/name :user/slug :user/picture])

(def ^:private post-attr-keys
  "Post attribute keys (namespaced)."
  [:post/id :post/title :post/content :post/author :post/tags :post/created-at :post/updated-at :post/featured?])

;;=============================================================================
;; Connection Management
;;=============================================================================

(def default-cfg
  "Default in-memory Datahike configuration."
  {:store {:backend :mem :id "blog"}
   :schema-flexibility :write
   :keep-history? true})

(defn- persistent-backend?
  "Is this a persistent backend (file or S3)?"
  [cfg]
  (#{:file :s3} (get-in cfg [:store :backend])))

(defn create-conn!
  "Create or connect to a Datahike database with blog schema.

   For :mem backend, always creates fresh database.
   For :file/:s3 backends, connects to existing or creates new."
  ([] (create-conn! default-cfg))
  ([cfg]
   (if (d/database-exists? cfg)
     ;; Database exists - connect (for persistent) or recreate (for mem)
     (if (persistent-backend? cfg)
       (d/connect cfg)
       (do
         (d/delete-database cfg)
         (d/create-database cfg)
         (let [conn (d/connect cfg)]
           (d/transact conn db-schema)
           conn)))
     ;; Database doesn't exist - create fresh
     (do
       (d/create-database cfg)
       (let [conn (d/connect cfg)]
         (d/transact conn db-schema)
         conn)))))

(defn database-empty?
  "Check if database has no posts."
  [conn]
  (zero? (or (d/q '[:find (count ?e) . :where [?e :post/id _]] @conn) 0)))

(defn release-conn!
  "Release a Datahike connection and optionally delete database.

   For persistent backends, only releases connection (preserves data).
   For :mem backend with cfg, deletes database.
   Handles protocol reload errors gracefully (common during REPL development)."
  ([conn] (try (d/release conn) (catch Exception _)))
  ([conn cfg]
   (try
     (d/release conn)
     (when-not (persistent-backend? cfg)
       (d/delete-database cfg))
     (catch IllegalArgumentException _
       ;; Protocol mismatch after reload - connection already invalid
       nil))))

;;=============================================================================
;; Entity Conversion (generic)
;;=============================================================================

(defn- normalize-user
  "Normalize Datahike user entity to consistent map."
  [entity]
  (when entity
    (into {} (for [[k v] entity :when (some #{k} user-attr-keys)]
               [k v]))))

(defn- normalize-post
  "Normalize Datahike post entity to consistent map.
   Expands :post/author ref to user map."
  [entity]
  (when entity
    (into {} (for [[k v] entity :when (some #{k} post-attr-keys)]
               [k (cond
                    (= k :post/author) (normalize-user v)
                    (set? v) (vec v)
                    :else v)]))))

(defn- prepare-post-for-db
  "Prepare post data for Datahike transaction.
   Converts tags to set, author user-id to lookup ref."
  [m]
  (into {} (for [[k v] m :when (some #{k} post-attr-keys)]
             [k (cond
                  (= k :post/tags) (set v)
                  ;; Convert user-id string to lookup ref
                  (and (= k :post/author) (string? v)) [:user/id v]
                  :else v)])))

;;=============================================================================
;; Markdown Content Handling
;;=============================================================================

(defn- extract-frontmatter
  "Extract properties from markdown frontmatter in content.
   Converts :author/:tags from frontmatter to :post/author/:post/tags.
   Strips frontmatter from content, keeping only the body."
  [data]
  (if-let [content (:post/content data)]
    (let [parsed (md/parse content)]
      (cond-> data
        ;; Strip frontmatter from content, keep only body
        (:content parsed) (assoc :post/content (:content parsed))
        ;; Extract author/tags to dedicated fields
        (:author parsed) (assoc :post/author (:author parsed))
        (:tags parsed) (assoc :post/tags (:tags parsed))))
    data))

;;=============================================================================
;; DataSource Implementation
;;=============================================================================

(defn- next-id [conn]
  (inc (or (d/q '[:find (max ?id) . :where [_ :post/id ?id]] @conn) 0)))

(defn- now [] (java.util.Date.))

(defn- find-by [conn query]
  (let [[k v] (first query)]
    (d/q `[:find (~'pull ~'?e [~'* {:post/author [~'*]}]) ~'.
           :in ~'$ ~'?v
           :where [~'?e ~k ~'?v]]
         @conn v)))

(defrecord PostsDataSource [conn]
  coll/DataSource
  (fetch [_ query]
    (try
      (mu/log ::db-fetch :entity :post :id (:post/id query))
      (normalize-post (find-by conn query))
      (catch Exception e
        (mu/log ::db-fetch-error :query query :error (ex-message e))
        (throw e))))

  (list-all [_]
    (try
      (mu/log ::db-list-all :entity :post)
      (->> (d/q '[:find [(pull ?e [* {:post/author [*]}]) ...] :where [?e :post/id _]] @conn)
           (map normalize-post)
           (sort-by :post/created-at #(compare %2 %1)))
      (catch Exception e
        (mu/log ::db-list-all-error :error (ex-message e))
        (throw e))))

  (create! [_ data]
    (try
      (let [ts (now)
            entity (merge (prepare-post-for-db (extract-frontmatter data))
                          {:post/id (next-id conn)
                           :post/created-at ts
                           :post/updated-at ts})]
        (d/transact conn [entity])
        (mu/log ::db-create :entity :post :id (:post/id entity))
        ;; Re-fetch to get expanded author
        (normalize-post (find-by conn {:post/id (:post/id entity)})))
      (catch Exception e
        (mu/log ::db-create-error :data data :error (ex-message e))
        (throw e))))

  (update! [this query data]
    (try
      (when-let [post (coll/fetch this query)]
        (let [updates (merge (prepare-post-for-db (extract-frontmatter data))
                             {:post/id (:post/id post)
                              :post/updated-at (now)})]
          (d/transact conn [updates])
          (mu/log ::db-update :entity :post :id (:post/id post))
          (coll/fetch this {:post/id (:post/id post)})))
      (catch Exception e
        (mu/log ::db-update-error :query query :data data :error (ex-message e))
        (throw e))))

  (delete! [this query]
    (try
      (if-let [post (coll/fetch this query)]
        (let [eid (d/q '[:find ?e . :in $ ?id :where [?e :post/id ?id]]
                       @conn (:post/id post))]
          (d/transact conn [[:db/retractEntity eid]])
          (mu/log ::db-delete :entity :post :id (:post/id post))
          true)
        false)
      (catch Exception e
        (mu/log ::db-delete-error :query query :error (ex-message e))
        (throw e)))))

;;=============================================================================
;; History Query
;;=============================================================================

(defn- find-by-at
  "Find entity by query at a specific database state."
  [db query]
  (let [[k v] (first query)]
    (d/q `[:find (~'pull ~'?e [~'*]) ~'.
           :in ~'$ ~'?v
           :where [~'?e ~k ~'?v]]
         db v)))

(defn post-history
  "Get all historical versions of a post by ID.
   Returns list of {:version/tx, :version/timestamp, :post/*} maps sorted newest first.
   Takes a db value (not conn) to ensure consistent view."
  [db post-id]
  (mu/log ::post-history-start :post-id post-id)
  (let [history-db (d/history db)
        ;; Find all transactions that touched this post's content or title
        txs (d/q '[:find ?tx ?inst
                   :in $ ?id
                   :where
                   [?e :post/id ?id]
                   (or [?e :post/content _ ?tx true]
                       [?e :post/title _ ?tx true])
                   [?tx :db/txInstant ?inst]]
                 history-db post-id)]
    (mu/log ::post-history-found :post-id post-id :tx-count (count txs))
    (->> txs
         (sort-by second)
         reverse
         (map (fn [[tx inst]]
                (let [db-at (d/as-of db tx)
                      post (normalize-post (find-by-at db-at {:post/id post-id}))]
                  (assoc post
                         :version/tx tx
                         :version/timestamp inst)))))))

;;=============================================================================
;; History Lookup (ILookup for API)
;;=============================================================================

(defn post-history-lookup
  "Create an ILookup for post history queries.
   Implements Wireable to serialize as nil (lazy lookup, not enumerable)."
  [conn]
  (reify
    clojure.lang.ILookup
    (valAt [_ query]
      (when-let [post-id (:post/id query)]
        (mu/log ::history-lookup :post-id post-id)
        (let [result (post-history @conn post-id)]
          (mu/log ::history-lookup-result :post-id post-id :count (count result))
          result)))
    (valAt [this query not-found]
      (or (.valAt this query) not-found))

    coll/Wireable
    (->wire [_] nil)))

;;=============================================================================
;; User Operations
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

(defn get-user-posts
  "Get all posts by a user ID."
  [conn user-id]
  (when user-id
    (->> (d/q '[:find [(pull ?p [* {:post/author [*]}]) ...]
                :in $ ?uid
                :where
                [?u :user/id ?uid]
                [?p :post/author ?u]]
              @conn user-id)
         (map normalize-post)
         (sort-by :post/created-at #(compare %2 %1)))))

(defn get-user-posts-by-slug
  "Get all posts by a user slug."
  [conn slug]
  (when slug
    (->> (d/q '[:find [(pull ?p [* {:post/author [*]}]) ...]
                :in $ ?slug
                :where
                [?u :user/slug ?slug]
                [?p :post/author ?u]]
              @conn slug)
         (map normalize-post)
         (sort-by :post/created-at #(compare %2 %1)))))

;;=============================================================================
;; Collection Constructor
;;=============================================================================

(defn posts
  "Create a posts collection for the given Datahike connection."
  ([conn] (posts conn {}))
  ([conn {:keys [indexes] :or {indexes #{#{:post/id}}} :as opts}]
   (coll/collection (->PostsDataSource conn) (assoc opts :indexes indexes))))

^:rct/test
(comment
  (def conn (create-conn!))

  ;; Slugify function
  (slugify "Bob Smith") ;=> "bob-smith"
  (slugify "Alice") ;=> "alice"
  (slugify "Bob O'Brien") ;=> "bob-o-brien"

  ;; User CRUD with auto-generated slug
  (create-user! conn #:user{:id "google-123" :email "alice@example.com" :name "Alice" :picture "http://pic"})
  (:user/name (get-user conn "google-123")) ;=> "Alice"
  (:user/slug (get-user conn "google-123")) ;=> "alice"

  ;; Get user by slug
  (:user/name (get-user-by-slug conn "alice")) ;=> "Alice"

  ;; Upsert user (update existing, slug stays stable)
  (upsert-user! conn #:user{:id "google-123" :email "alice@example.com" :name "Alice Updated" :picture "http://pic2"})
  (:user/name (get-user conn "google-123")) ;=> "Alice Updated"
  (:user/slug (get-user conn "google-123")) ;=> "alice"

  ;; Duplicate name gets unique slug
  (create-user! conn #:user{:id "google-789" :email "alice2@example.com" :name "Alice" :picture ""})
  (:user/slug (get-user conn "google-789")) ;=> "alice-2"

  ;; Create post with user reference
  (def p (posts conn))
  (:post/id (coll/mutate! p nil {:post/title "Test" :post/content "Hello" :post/author "google-123"})) ;=> 1
  (:post/title (get p {:post/id 1})) ;=> "Test"

  ;; Author is expanded to user map with slug
  (:user/name (:post/author (get p {:post/id 1}))) ;=> "Alice Updated"
  (:user/slug (:post/author (get p {:post/id 1}))) ;=> "alice"

  ;; Create another user and post
  (create-user! conn #:user{:id "google-456" :email "bob@example.com" :name "Bob" :picture "http://bob"})
  (coll/mutate! p nil {:post/title "Second" :post/content "Two" :post/author "google-456"})
  (count (seq p)) ;=> 2

  ;; Get posts by user ID
  (count (get-user-posts conn "google-123")) ;=> 1
  (count (get-user-posts conn "google-456")) ;=> 1

  ;; Get posts by user slug
  (count (get-user-posts-by-slug conn "alice")) ;=> 1
  (count (get-user-posts-by-slug conn "bob")) ;=> 1

  ;; Post without author
  (coll/mutate! p nil {:post/title "Anonymous" :post/content "No author"})
  (:post/author (get p {:post/id 3})) ;=> nil

  ;; Update and delete
  (:post/title (coll/mutate! p {:post/id 1} {:post/title "Updated"})) ;=> "Updated"
  (coll/mutate! p {:post/id 1} nil) ;=> true
  (count p) ;=> 2

  ;; Database empty check
  (database-empty? conn) ;=> false

  ;; Persistent backend detection
  (boolean (#'persistent-backend? {:store {:backend :mem}})) ;=> false
  (boolean (#'persistent-backend? {:store {:backend :file :path "/tmp/db"}})) ;=> true
  (boolean (#'persistent-backend? {:store {:backend :s3 :bucket "my-bucket"}})) ;=> true

  (release-conn! conn))

^:rct/test
(comment
  ;; === Chinese name support via pinyin ===
  (def conn (create-conn!))

  ;; Slugify handles Chinese via pinyin (using generic test name)
  (slugify "张伟") ;=> "zhangwei"
  (slugify "李明") ;=> "liming"
  (slugify "Bob Smith") ;=> "bob-smith"

  ;; Create user with Chinese name
  (create-user! conn #:user{:id "google-cn-123" :email "zhang@example.com" :name "张伟" :picture ""})
  (:user/slug (get-user conn "google-cn-123")) ;=> "zhangwei"
  (:user/name (get-user-by-slug conn "zhangwei")) ;=> "张伟"

  ;; Get user by name
  (:user/id (get-user-by-name conn "张伟")) ;=> "google-cn-123"
  (get-user-by-name conn "Nonexistent") ;=> nil

  (release-conn! conn))

^:rct/test
(comment
  ;; === Placeholder user claiming ===
  (def conn (create-conn!))

  ;; Create a placeholder user (simulating import)
  ;; Placeholder users have user/id starting with "placeholder:"
  (d/transact conn [{:user/id "placeholder:zhangwei"
                     :user/slug "zhangwei"
                     :user/name "张伟"
                     :user/email ""
                     :user/picture ""}])

  ;; Verify placeholder exists (detected by ID prefix)
  (str/starts-with? (:user/id (get-user conn "placeholder:zhangwei")) "placeholder:") ;=> true
  (some? (get-placeholder-by-name conn "张伟")) ;=> true

  ;; Create a post by the placeholder user
  (def p (posts conn))
  (coll/mutate! p nil {:post/title "Chinese Post" :post/content "Hello" :post/author "placeholder:zhangwei"})
  (:user/name (:post/author (get p {:post/id 1}))) ;=> "张伟"

  ;; Now simulate Google login with same name - should claim placeholder
  (upsert-user! conn #:user{:id "google-real-123" :email "zhang@flybot.sg" :name "张伟" :picture "http://pic"})

  ;; Placeholder should be gone, real user should exist
  (get-user conn "placeholder:zhangwei") ;=> nil
  (:user/email (get-user conn "google-real-123")) ;=> "zhang@flybot.sg"
  (str/starts-with? (:user/id (get-user conn "google-real-123")) "placeholder:") ;=> false

  ;; Post should now link to the real user (same entity, updated user-id)
  (:user/id (:post/author (get p {:post/id 1}))) ;=> "google-real-123"

  ;; Subsequent logins should update normally (no claiming)
  (upsert-user! conn #:user{:id "google-real-123" :email "zhang@flybot.sg" :name "张伟 Updated" :picture "http://pic2"})
  (:user/name (get-user conn "google-real-123")) ;=> "张伟 Updated"
  (:user/slug (get-user conn "google-real-123")) ;=> "zhangwei"

  (release-conn! conn))

;;=============================================================================
;; Seed Data
;;=============================================================================

(defn seed!
  "Seed database with sample data.
   Creates sample users and posts with proper user references."
  [conn]
  ;; Create sample users (all @flybot.sg for consistent email pattern)
  (create-user! conn #:user{:id "sample-alice" :email "alice@flybot.sg" :name "Alice Johnson" :picture ""})
  (create-user! conn #:user{:id "sample-bob" :email "bob@flybot.sg" :name "Bob Smith" :picture ""})
  (create-user! conn #:user{:id "sample-zhang" :email "zhang@flybot.sg" :name "张伟" :picture ""})

  (let [ds (->PostsDataSource conn)]
    ;; Home page content (featured = hero post)
    (coll/create! ds {:post/title "Welcome to Flybot"
                      :post/author "sample-alice"
                      :post/tags ["Home" "clojure"]
                      :post/featured? true
                      :post/content "# Building the Future of Software

At Flybot, we're passionate about creating elegant solutions using **functional programming** and **data-driven design**.

## What We Do

- **Custom Software Development** - Tailored solutions for your business needs
- **Technical Consulting** - Expert guidance on architecture and best practices
- **Training & Workshops** - Level up your team's Clojure skills

*Innovation through simplicity.*"})

    ;; Additional Home posts for slideshow
    (coll/create! ds {:post/title "Latest News: Q1 2026 Update"
                      :post/author "sample-bob"
                      :post/tags ["Home" "news"]
                      :post/content "We're excited to announce several new client partnerships and the release of our open-source pull-pattern library!"})

    (coll/create! ds {:post/title "Featured Project: Data Pipeline"
                      :post/author "sample-alice"
                      :post/tags ["Home" "projects" "tech" "clojure"]
                      :post/content "Check out our latest case study on building high-performance data pipelines with Clojure and Kafka."})

    ;; About page content (featured = hero post)
    (coll/create! ds {:post/title "About Flybot"
                      :post/author "sample-alice"
                      :post/tags ["About"]
                      :post/featured? true
                      :post/content "# Our Story

Founded in Singapore, Flybot is a software consultancy specializing in **Clojure** and **functional programming**.

## Our Philosophy

We believe that **simple tools** create **powerful solutions**. Our approach combines:

- **Data-oriented design** - Treating code as data transformation
- **REPL-driven development** - Fast feedback loops for rapid iteration
- **Immutable-first architecture** - Reliable, predictable systems

## The Team

Our engineers bring decades of combined experience from top tech companies, united by a shared love for elegant code and pragmatic solutions.

> \"Simplicity is the ultimate sophistication.\" - Leonardo da Vinci"})

    (coll/create! ds {:post/title "Our Tech Stack"
                      :post/author "sample-bob"
                      :post/tags ["About" "tech" "clojure"]
                      :post/content "We work primarily with Clojure/ClojureScript, Datomic, and modern cloud infrastructure. Our tooling philosophy emphasizes composability and data-driven configuration."})

    ;; Apply page content (featured = hero post)
    (coll/create! ds {:post/title "Join Our Team"
                      :post/author "sample-bob"
                      :post/tags ["Apply"]
                      :post/featured? true
                      :post/content "# We're Hiring!

Looking for talented engineers who love functional programming? Join us!

## Open Positions

### Senior Clojure Engineer
Build distributed systems and APIs using Clojure, Kafka, and PostgreSQL.

### Full-Stack Developer
Create beautiful UIs with ClojureScript and Replicant while building robust backends.

### DevOps Engineer
Design and maintain our cloud infrastructure on AWS/GCP.

## What We Offer

- Remote-first culture
- Learning budget for conferences and courses
- Flexible PTO policy
- Latest hardware and tools

## How to Apply

Send your resume and a brief note about your favorite Clojure project to **careers@flybot.sg**"})

    (coll/create! ds {:post/title "Internship Program"
                      :post/author "sample-alice"
                      :post/tags ["Apply" "internship"]
                      :post/content "Our 3-month internship program is designed for students and early-career developers eager to learn functional programming. Interns work on real projects alongside senior engineers."})

    ;; Regular blog posts
    (coll/create! ds {:post/title "Understanding Pull Patterns"
                      :post/author "sample-alice"
                      :post/tags ["clojure" "patterns"]
                      :post/content "Pull patterns let you declaratively specify what data you want from nested data structures. Instead of writing imperative traversal code, you describe the shape of data you need and let the pattern engine do the work."})

    (coll/create! ds {:post/title "Building APIs with Lazy Data"
                      :post/author "sample-bob"
                      :post/tags ["clojure" "api"]
                      :post/content "The key insight is that your API is just a lazy data structure. By implementing ILookup, you can create collections that fetch data on-demand, making your API both flexible and efficient."})

    (coll/create! ds {:post/title "REPL-Driven Development Tips"
                      :post/author "sample-alice"
                      :post/tags ["clojure" "workflow"]
                      :post/content "Rich comment blocks (^:rct/test) combine documentation, examples, and tests in one place. They're evaluated during development but ignored in production."})

    (coll/create! ds {:post/title "Clojure中的函数式编程"
                      :post/author "sample-zhang"
                      :post/tags ["clojure" "functional"]
                      :post/content "Functional programming in Clojure emphasizes immutability and pure functions. This approach leads to code that is easier to test, reason about, and parallelize."})))
