(ns sg.flybot.blog.db
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
   [sg.flybot.pullable.collection :as coll]
   [sg.flybot.blog.markdown :as md]))

;;=============================================================================
;; Schema
;;=============================================================================

(def post-schema
  "Datahike schema for posts.

   Content is stored as markdown with YAML frontmatter containing author/tags.
   Author/tags are also indexed separately for queries."
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
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :post/tags
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}
   {:db/ident :post/created-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :post/updated-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}])

(def ^:private attr-keys
  "Post attribute keys (without namespace)."
  [:id :title :content :author :tags :created-at :updated-at])

;;=============================================================================
;; Connection Management
;;=============================================================================

(def default-cfg
  "Default in-memory Datahike configuration."
  {:store {:backend :mem :id "blog"}
   :schema-flexibility :write
   :keep-history? true})

(defn create-conn!
  "Create a new Datahike connection with blog schema."
  ([] (create-conn! default-cfg))
  ([cfg]
   (when (d/database-exists? cfg)
     (d/delete-database cfg))
   (d/create-database cfg)
   (let [conn (d/connect cfg)]
     (d/transact conn post-schema)
     conn)))

(defn release-conn!
  "Release a Datahike connection and optionally delete database."
  ([conn] (d/release conn))
  ([conn cfg]
   (d/release conn)
   (d/delete-database cfg)))

;;=============================================================================
;; Entity Conversion (generic)
;;=============================================================================

(defn- strip-ns
  "Convert {:post/id 1 :post/title \"x\"} -> {:id 1 :title \"x\"}."
  [entity]
  (when entity
    (into {} (for [[k v] entity
                   :when (= "post" (namespace k))]
               [(keyword (name k)) (if (set? v) (vec v) v)]))))

(defn- add-ns
  "Convert {:id 1 :title \"x\"} -> {:post/id 1 :post/title \"x\"}."
  [m]
  (into {} (for [[k v] m :when (some #{k} attr-keys)]
             [(keyword "post" (name k)) (if (= k :tags) (set v) v)])))

;;=============================================================================
;; Markdown Content Handling
;;=============================================================================

(defn- extract-frontmatter
  "Extract properties from markdown frontmatter in content."
  [data]
  (if-let [content (:content data)]
    (merge data (dissoc (md/parse content) :content))
    data))

;;=============================================================================
;; DataSource Implementation
;;=============================================================================

(defn- next-id [conn]
  (inc (or (d/q '[:find (max ?id) . :where [_ :post/id ?id]] @conn) 0)))

(defn- now [] (java.util.Date.))

(defn- find-by [conn query]
  (let [[k v] (first query)
        attr (keyword "post" (name k))]
    (d/q `[:find (~'pull ~'?e [~'*]) ~'.
           :in ~'$ ~'?v
           :where [~'?e ~attr ~'?v]]
         @conn v)))

(defrecord PostsDataSource [conn]
  coll/DataSource
  (fetch [_ query]
    (strip-ns (find-by conn query)))

  (list-all [_]
    (->> (d/q '[:find [(pull ?e [*]) ...] :where [?e :post/id _]] @conn)
         (map strip-ns)
         (sort-by :created-at #(compare %2 %1))))

  (create! [_ data]
    (let [ts (now)
          entity (merge (add-ns (extract-frontmatter data))
                        {:post/id (next-id conn)
                         :post/created-at ts
                         :post/updated-at ts})]
      (d/transact conn [entity])
      (strip-ns entity)))

  (update! [this query data]
    (when-let [post (coll/fetch this query)]
      (let [updates (merge (add-ns (extract-frontmatter data))
                           {:post/id (:id post)
                            :post/updated-at (now)})]
        (d/transact conn [updates])
        (coll/fetch this {:id (:id post)}))))

  (delete! [this query]
    (if-let [post (coll/fetch this query)]
      (let [eid (d/q '[:find ?e . :in $ ?id :where [?e :post/id ?id]]
                     @conn (:id post))]
        (d/transact conn [[:db/retractEntity eid]])
        true)
      false)))

;;=============================================================================
;; Collection Constructor
;;=============================================================================

(defn posts
  "Create a posts collection for the given Datahike connection."
  ([conn] (posts conn {}))
  ([conn opts]
   (coll/collection (->PostsDataSource conn) opts)))

^:rct/test
(comment
  (def conn (create-conn!))
  (def p (posts conn))

  ;; Create with markdown frontmatter
  (:id (coll/mutate! p nil {:title "Test" :content "---\nauthor: Me\n---\n\nHello"})) ;=> 1
  (:title (get p {:id 1})) ;=> "Test"
  (:author (get p {:id 1})) ;=> "Me"

  ;; Frontmatter with tags
  (coll/mutate! p nil {:title "Second" :content "---\nauthor: You\ntags:\n  - test\n---\n\nTwo"})
  (count (seq p)) ;=> 2
  (:author (get p {:id 2})) ;=> "You"
  (:tags (get p {:id 2})) ;=> ["test"]

  ;; Plain content (no frontmatter, no author/tags)
  (coll/mutate! p nil {:title "Plain" :content "Just text"})
  (:author (get p {:id 3})) ;=> nil

  ;; Update and delete
  (:title (coll/mutate! p {:id 1} {:title "Updated"})) ;=> "Updated"
  (coll/mutate! p {:id 1} nil) ;=> true
  (count p) ;=> 2

  ;; Index enforcement
  (def p2 (posts conn {:indexes #{#{:id} #{:author}}}))
  (:title (get p2 {:author "You"})) ;=> "Second"

  (release-conn! conn))

;;=============================================================================
;; Seed Data
;;=============================================================================

(defn seed!
  "Seed database with sample data using markdown format."
  [conn]
  (let [ds (->PostsDataSource conn)]
    (coll/create! ds {:title "Welcome to My Blog"
                      :content "---
author: Alice
tags:
  - welcome
  - meta
---

This is my first post using the pull-based API!"})
    (coll/create! ds {:title "Understanding Pull Patterns"
                      :content "---
author: Alice
tags:
  - clojure
  - patterns
---

Pull patterns let you declaratively specify what data you want..."})
    (coll/create! ds {:title "Building APIs with Lazy Data"
                      :content "---
author: Bob
tags:
  - clojure
  - api
---

The key insight is that your API is just a lazy data structure..."})))
