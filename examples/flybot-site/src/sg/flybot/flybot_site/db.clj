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
   [sg.flybot.pullable.collection :as coll]
   [sg.flybot.flybot-site.markdown :as md]
   [sg.flybot.flybot-site.log :as log]))

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
  "Post attribute keys (namespaced)."
  [:post/id :post/title :post/content :post/author :post/tags :post/created-at :post/updated-at])

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

(defn- normalize-entity
  "Normalize Datahike entity to consistent map with vectors for multi-cardinality."
  [entity]
  (when entity
    (into {} (for [[k v] entity :when (some #{k} attr-keys)]
               [k (if (set? v) (vec v) v)]))))

(defn- prepare-for-db
  "Prepare data for Datahike transaction, converting tags to set."
  [m]
  (into {} (for [[k v] m :when (some #{k} attr-keys)]
             [k (if (= k :post/tags) (set v) v)])))

;;=============================================================================
;; Markdown Content Handling
;;=============================================================================

(defn- extract-frontmatter
  "Extract properties from markdown frontmatter in content.
   Converts :author/:tags from frontmatter to :post/author/:post/tags."
  [data]
  (if-let [content (:post/content data)]
    (let [parsed (md/parse content)]
      (cond-> data
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
    (d/q `[:find (~'pull ~'?e [~'*]) ~'.
           :in ~'$ ~'?v
           :where [~'?e ~k ~'?v]]
         @conn v)))

(defrecord PostsDataSource [conn]
  coll/DataSource
  (fetch [_ query]
    (try
      (log/log-db-op :fetch :post (:post/id query))
      (normalize-entity (find-by conn query))
      (catch Exception e
        (log/error "DB fetch failed:" {:query query :error (ex-message e)})
        (throw e))))

  (list-all [_]
    (try
      (log/log-db-op :list-all :post nil)
      (->> (d/q '[:find [(pull ?e [*]) ...] :where [?e :post/id _]] @conn)
           (map normalize-entity)
           (sort-by :post/created-at #(compare %2 %1)))
      (catch Exception e
        (log/error "DB list-all failed:" {:error (ex-message e)})
        (throw e))))

  (create! [_ data]
    (try
      (let [ts (now)
            entity (merge (prepare-for-db (extract-frontmatter data))
                          {:post/id (next-id conn)
                           :post/created-at ts
                           :post/updated-at ts})]
        (d/transact conn [entity])
        (log/log-db-create :post entity)
        (normalize-entity entity))
      (catch Exception e
        (log/error "DB create failed:" {:data data :error (ex-message e)})
        (throw e))))

  (update! [this query data]
    (try
      (when-let [post (coll/fetch this query)]
        (let [updates (merge (prepare-for-db (extract-frontmatter data))
                             {:post/id (:post/id post)
                              :post/updated-at (now)})]
          (d/transact conn [updates])
          (log/log-db-update :post (:post/id post))
          (coll/fetch this {:post/id (:post/id post)})))
      (catch Exception e
        (log/error "DB update failed:" {:query query :data data :error (ex-message e)})
        (throw e))))

  (delete! [this query]
    (try
      (if-let [post (coll/fetch this query)]
        (let [eid (d/q '[:find ?e . :in $ ?id :where [?e :post/id ?id]]
                       @conn (:post/id post))]
          (d/transact conn [[:db/retractEntity eid]])
          (log/log-db-delete :post (:post/id post))
          true)
        false)
      (catch Exception e
        (log/error "DB delete failed:" {:query query :error (ex-message e)})
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
  (log/debug "post-history called for post:" post-id)
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
    (log/debug "Found" (count txs) "transaction(s) for post" post-id)
    (->> txs
         (sort-by second)
         reverse
         (map (fn [[tx inst]]
                (let [db-at (d/as-of db tx)
                      post (normalize-entity (find-by-at db-at {:post/id post-id}))]
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
        (log/debug "History lookup for post:" post-id)
        (let [result (post-history @conn post-id)]
          (log/debug "History result count:" (count result))
          result)))
    (valAt [this query not-found]
      (or (.valAt this query) not-found))

    coll/Wireable
    (->wire [_] nil)))

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
  (def p (posts conn))

  ;; Create with markdown frontmatter
  (:post/id (coll/mutate! p nil {:post/title "Test" :post/content "---\nauthor: Me\n---\n\nHello"})) ;=> 1
  (:post/title (get p {:post/id 1})) ;=> "Test"
  (:post/author (get p {:post/id 1})) ;=> "Me"

  ;; Frontmatter with tags
  (coll/mutate! p nil {:post/title "Second" :post/content "---\nauthor: You\ntags:\n  - test\n---\n\nTwo"})
  (count (seq p)) ;=> 2
  (:post/author (get p {:post/id 2})) ;=> "You"
  (:post/tags (get p {:post/id 2})) ;=> ["test"]

  ;; Plain content (no frontmatter, no author/tags)
  (coll/mutate! p nil {:post/title "Plain" :post/content "Just text"})
  (:post/author (get p {:post/id 3})) ;=> nil

  ;; Update and delete
  (:post/title (coll/mutate! p {:post/id 1} {:post/title "Updated"})) ;=> "Updated"
  (coll/mutate! p {:post/id 1} nil) ;=> true
  (count p) ;=> 2

  ;; Index enforcement
  (def p2 (posts conn {:indexes #{#{:post/id} #{:post/author}}}))
  (:post/title (get p2 {:post/author "You"})) ;=> "Second"

  (release-conn! conn))

;;=============================================================================
;; Seed Data
;;=============================================================================

(defn seed!
  "Seed database with sample data using markdown format."
  [conn]
  (let [ds (->PostsDataSource conn)]
    ;; Home page content
    (coll/create! ds {:post/title "Welcome to Flybot"
                      :post/content "---
author: Flybot Team
tags:
  - Home
---

# Building the Future of Software

At Flybot, we're passionate about creating elegant solutions using **functional programming** and **data-driven design**.

## What We Do

- **Custom Software Development** - Tailored solutions for your business needs
- **Technical Consulting** - Expert guidance on architecture and best practices
- **Training & Workshops** - Level up your team's Clojure skills

*Innovation through simplicity.*"})

    ;; Additional Home posts for slideshow
    (coll/create! ds {:post/title "Latest News: Q1 2026 Update"
                      :post/content "---
author: Flybot Team
tags:
  - Home
  - news
---

We're excited to announce several new client partnerships and the release of our open-source pull-pattern library!"})

    (coll/create! ds {:post/title "Featured Project: Data Pipeline"
                      :post/content "---
author: Alice
tags:
  - Home
  - projects
---

Check out our latest case study on building high-performance data pipelines with Clojure and Kafka."})

    ;; About page content
    (coll/create! ds {:post/title "About Flybot"
                      :post/content "---
author: Flybot Team
tags:
  - About
---

# Our Story

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
                      :post/content "---
author: Bob
tags:
  - About
  - tech
---

We work primarily with Clojure/ClojureScript, Datomic, and modern cloud infrastructure. Our tooling philosophy emphasizes composability and data-driven configuration."})

    ;; Apply page content
    (coll/create! ds {:post/title "Join Our Team"
                      :post/content "---
author: Flybot Team
tags:
  - Apply
---

# We're Hiring!

Looking for talented engineers who love functional programming? Join us!

## Open Positions

### Senior Clojure Engineer
Build distributed systems and APIs using Clojure, Kafka, and PostgreSQL.

### Full-Stack Developer
Create beautiful UIs with ClojureScript and Replicant while building robust backends.

### DevOps Engineer
Design and maintain our cloud infrastructure on AWS/GCP.

## What We Offer

- 🏠 **Remote-first** culture
- 📚 **Learning budget** for conferences and courses
- 🌴 **Flexible PTO** policy
- 💻 **Latest hardware** and tools

## How to Apply

Send your resume and a brief note about your favorite Clojure project to **careers@flybot.sg**"})

    (coll/create! ds {:post/title "Internship Program"
                      :post/content "---
author: Alice
tags:
  - Apply
  - internship
---

Our 3-month internship program is designed for students and early-career developers eager to learn functional programming. Interns work on real projects alongside senior engineers."})

    ;; Regular blog posts
    (coll/create! ds {:post/title "Understanding Pull Patterns"
                      :post/content "---
author: Alice
tags:
  - clojure
  - patterns
---

Pull patterns let you declaratively specify what data you want from nested data structures. Instead of writing imperative traversal code, you describe the shape of data you need and let the pattern engine do the work."})

    (coll/create! ds {:post/title "Building APIs with Lazy Data"
                      :post/content "---
author: Bob
tags:
  - clojure
  - api
---

The key insight is that your API is just a lazy data structure. By implementing ILookup, you can create collections that fetch data on-demand, making your API both flexible and efficient."})

    (coll/create! ds {:post/title "REPL-Driven Development Tips"
                      :post/content "---
author: Alice
tags:
  - clojure
  - workflow
---

Rich comment blocks (^:rct/test) combine documentation, examples, and tests in one place. They're evaluated during development but ignored in production."})))
