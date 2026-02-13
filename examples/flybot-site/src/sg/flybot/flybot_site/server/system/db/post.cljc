(ns sg.flybot.flybot-site.server.system.db.post
  "Posts DataSource implementation.

   Implements DataSource for CRUD operations on blog posts using Datahike.
   Handles featured post logic (at most one featured per page) and post history."
  (:require
   [datahike.api :as d]
   [sg.flybot.pullable.collection :as coll]
   [sg.flybot.flybot-site.server.system.db.markdown :as markdown]
   [com.brunobonacci.mulog :as mu]))

;;=============================================================================
;; Entity Conversion
;;=============================================================================

(def ^:private post-attr-keys
  "Post attribute keys (namespaced)."
  [:post/id :post/title :post/content :post/author :post/tags :post/pages :post/created-at :post/updated-at :post/featured?])

(def ^:private user-display-keys
  "User keys needed for post author display."
  [:user/email :user/name :user/slug :user/picture])

(defn- normalize-user-for-post
  "Normalize Datahike user entity to consistent map for embedding in post."
  [entity]
  (when entity
    (select-keys entity user-display-keys)))

(defn- normalize-post
  "Normalize Datahike post entity to consistent map.
   Expands :post/author ref to user map."
  [entity]
  (when entity
    (-> (select-keys entity post-attr-keys)
        (cond-> (:post/author entity)  (update :post/author normalize-user-for-post))
        (cond-> (set? (:post/tags entity))  (update :post/tags vec))
        (cond-> (set? (:post/pages entity)) (update :post/pages vec)))))

(defn- prepare-post-for-db
  "Prepare post data for Datahike transaction.
   Converts tags to set, author user-id to lookup ref."
  [m]
  (-> (select-keys m post-attr-keys)
      (cond-> (contains? m :post/tags)  (update :post/tags set))
      (cond-> (contains? m :post/pages) (update :post/pages set))
      (cond-> (and (contains? m :post/author) (string? (:post/author m)))
        (update :post/author (fn [v] [:user/id v])))))

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

(defn- unfeature-siblings!
  "When a post becomes featured, un-feature other featured posts that share
   any of its pages. Ensures at most one featured (hero) post per page."
  [conn post-id post-pages]
  (when (seq post-pages)
    (let [siblings (d/q '[:find [?id ...]
                          :in $ ?self-id [?page ...]
                          :where
                          [?e :post/featured? true]
                          [?e :post/id ?id]
                          [(not= ?id ?self-id)]
                          [?e :post/pages ?page]]
                        @conn post-id post-pages)]
      (when (seq siblings)
        (d/transact conn (mapv #(vector :db/add [:post/id %] :post/featured? false)
                               siblings))
        (mu/log ::unfeature-siblings :post-id post-id :unfeatured siblings)))))

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
      (->> (d/q '[:find [(pull ?e [* {:post/author [*]}]) ...]
                  :where [?e :post/id _]]
                @conn)
           (map normalize-post)
           (sort-by :post/created-at #(compare %2 %1)))
      (catch Exception e
        (mu/log ::db-list-all-error :error (ex-message e))
        (throw e))))

  (create! [_ data]
    (try
      (let [ts (now)
            entity (merge (prepare-post-for-db (markdown/extract-frontmatter data))
                          {:post/id (next-id conn)
                           :post/created-at ts
                           :post/updated-at ts})]
        (d/transact conn [entity])
        (when (:post/featured? entity)
          (unfeature-siblings! conn (:post/id entity) (:post/pages entity)))
        (mu/log ::db-create :entity :post :id (:post/id entity))
        ;; Re-fetch to get expanded author
        (normalize-post (find-by conn {:post/id (:post/id entity)})))
      (catch Exception e
        (mu/log ::db-create-error :data data :error (ex-message e))
        (throw e))))

  (update! [this query data]
    (try
      (when-let [post (coll/fetch this query)]
        (let [updates (merge (prepare-post-for-db (markdown/extract-frontmatter data))
                             {:post/id (:post/id post)
                              :post/updated-at (now)})
              eid (d/q '[:find ?e . :in $ ?id :where [?e :post/id ?id]]
                       @conn (:post/id post))
              ;; Retract old cardinality-many values so updates replace (not accumulate)
              retractions (concat
                           (for [t (:post/tags post)] [:db/retract eid :post/tags t])
                           (for [p (:post/pages post)] [:db/retract eid :post/pages p]))]
          (d/transact conn (into (vec retractions) [updates]))
          (when (:post/featured? updates)
            (unfeature-siblings! conn (:post/id post)
                                 (or (:post/pages updates) (:post/pages post))))
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
    (d/q `[:find (~'pull ~'?e [~'* {:post/author [~'*]}]) ~'.
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

^:rct/test
(comment
  (require '[sg.flybot.flybot-site.server.system.db :as db])

  (def conn (db/create-conn!))
  (db/create-user! conn #:user{:id "u1" :email "a@b.com" :name "Alice" :picture ""})
  (def p (db/posts conn))
  (coll/mutate! p nil {:post/title "V1" :post/content "first" :post/author "u1"})
  (coll/mutate! p {:post/id 1} {:post/title "V2" :post/content "second"})

  (count (post-history @conn 1)) ;=> 2
  (:user/name (:post/author (first (post-history @conn 1)))) ;=> "Alice"

  (db/release-conn! conn))

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
;; Collection Constructor
;;=============================================================================

(defn posts
  "Create a posts collection for the given Datahike connection.
   When a post is marked featured, other featured posts sharing a page are un-featured."
  ([conn] (posts conn {}))
  ([conn {:keys [indexes] :or {indexes #{#{:post/id}}} :as opts}]
   (coll/collection (->PostsDataSource conn) (assoc opts :id-key :post/id :indexes indexes))))
