(ns sg.flybot.flybot-site.server.system.db
  "Datahike-backed blog database.

   Facade namespace: owns schema and connection management, re-exports
   domain functions from child namespaces (db.post, db.user, db.role).

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
   [sg.flybot.flybot-site.server.system.db.post :as post]
   [sg.flybot.flybot-site.server.system.db.user :as user]
   [sg.flybot.flybot-site.server.system.db.role :as role]))

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
    :db/cardinality :db.cardinality/one}
   {:db/ident :user/roles
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true}])

(def role-schema
  "Datahike schema for user roles.

   Roles are component entities owned by users. Each role has a name keyword
   and a grant timestamp. Valid role names: :member, :admin, :owner."
  [{:db/ident :role/name
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :role/granted-at
    :db/valueType :db.type/instant
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
   {:db/ident :post/pages
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
  (concat user-schema role-schema post-schema))

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
   For :file/:s3 backends, connects to existing and ensures schema is up to date."
  ([] (create-conn! default-cfg))
  ([cfg]
   (if (d/database-exists? cfg)
     ;; Database exists - connect (for persistent) or recreate (for mem)
     (if (persistent-backend? cfg)
       (let [conn (d/connect cfg)]
         (d/transact conn db-schema)
         conn)
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
;; Re-exports: db.post
;;=============================================================================

(def ->PostsDataSource post/->PostsDataSource)
(def posts post/posts)
(def post-history post/post-history)
(def post-history-lookup post/post-history-lookup)

;;=============================================================================
;; Re-exports: db.user
;;=============================================================================

(def slugify user/slugify)
(def get-user user/get-user)
(def get-user-by-name user/get-user-by-name)
(def create-user! user/create-user!)
(def upsert-user! user/upsert-user!)
(def count-user-posts user/count-user-posts)
(def count-user-revisions user/count-user-revisions)
(def users user/users)

;;=============================================================================
;; Re-exports: db.role
;;=============================================================================

(def get-user-roles role/get-user-roles)
(def get-user-roles-detailed role/get-user-roles-detailed)
(def grant-role! role/grant-role!)
(def user-roles role/user-roles)

;;=============================================================================
;; Integration Tests (cross-domain, require conn + multiple children)
;;=============================================================================

^:rct/test
(comment
  ;; --- User CRUD ---
  (def conn (create-conn!))

  (create-user! conn #:user{:id "u1" :email "a@b.com" :name "Alice" :picture ""})
  (:user/slug (get-user conn "u1")) ;=> "alice"
  (:user/name (user/get-user-by-slug conn "alice")) ;=> "Alice"
  (:user/id (get-user-by-name conn "Alice")) ;=> "u1"

  ;; Duplicate name gets suffixed slug
  (create-user! conn #:user{:id "u2" :email "b@b.com" :name "Alice" :picture ""})
  (:user/slug (get-user conn "u2")) ;=> "alice-2"

  ;; Upsert updates name but preserves slug
  (upsert-user! conn #:user{:id "u1" :email "a@b.com" :name "Alice Updated" :picture ""})
  (:user/name (get-user conn "u1")) ;=> "Alice Updated"
  (:user/slug (get-user conn "u1")) ;=> "alice"

  ;; Upsert claims matching placeholder
  (d/transact conn [{:user/id "placeholder:z" :user/slug "zhang" :user/name "Zhang"
                     :user/email "" :user/picture ""}])
  (some? (user/get-placeholder-by-name conn "Zhang")) ;=> true
  (upsert-user! conn #:user{:id "real-z" :email "z@b.com" :name "Zhang" :picture ""})
  (get-user conn "placeholder:z") ;=> nil
  (:user/email (get-user conn "real-z")) ;=> "z@b.com"

  (release-conn! conn))

^:rct/test
(comment
  ;; --- Count posts/revisions ---
  (def conn (create-conn!))
  (create-user! conn #:user{:id "u1" :email "a@b.com" :name "A" :picture ""})
  (def p (posts conn))
  (coll/mutate! p nil {:post/title "P1" :post/content "x" :post/author "u1"})
  (coll/mutate! p nil {:post/title "P2" :post/content "y" :post/author "u1"})

  (count-user-posts conn "u1") ;=> 2
  (count-user-posts conn nil) ;=> 0

  ;; No edits yet
  (count-user-revisions conn "u1") ;=> 0

  ;; Edit content -> 1 revision
  (coll/mutate! p {:post/id 1} {:post/title "P1" :post/content "x edited"})
  (count-user-revisions conn "u1") ;=> 1

  ;; Edit title of other post -> 2 revisions
  (coll/mutate! p {:post/id 2} {:post/title "P2 renamed" :post/content "y"})
  (count-user-revisions conn "u1") ;=> 2

  (release-conn! conn))

^:rct/test
(comment
  ;; --- Role operations ---
  (def conn (create-conn!))
  (create-user! conn #:user{:id "u1" :email "a@b.com" :name "A" :picture ""})

  (get-user-roles conn "u1") ;=> #{}

  (grant-role! conn "u1" :member)
  (get-user-roles conn "u1") ;=> #{:member}

  ;; Idempotent
  (grant-role! conn "u1" :member)
  (get-user-roles conn "u1") ;=> #{:member}

  (grant-role! conn "u1" :admin)
  (map :role/name (get-user-roles-detailed conn "u1")) ;=> [:member :admin]

  (role/revoke-role! conn "u1" :admin)
  (get-user-roles conn "u1") ;=> #{:member}

  ;; list-users includes roles
  (-> (user/list-users conn) first :user/roles) ;=> #{:member}

  (release-conn! conn))

^:rct/test
(comment
  ;; --- Posts collection CRUD + featured ---
  (def conn (create-conn!))
  (create-user! conn #:user{:id "u1" :email "a@b.com" :name "A" :picture ""})
  (def p (posts conn))

  ;; CRUD
  (:post/id (coll/mutate! p nil {:post/title "T" :post/content "x" :post/author "u1"})) ;=> 1
  (:post/title (get p {:post/id 1})) ;=> "T"
  (:user/slug (:post/author (get p {:post/id 1}))) ;=> "a"
  (:post/title (coll/mutate! p {:post/id 1} {:post/title "Updated"})) ;=> "Updated"

  ;; Featured: only one per page
  (coll/mutate! p nil {:post/title "Hero" :post/content "x" :post/author "u1"
                       :post/featured? true :post/pages ["Home"]})
  (coll/mutate! p nil {:post/title "New Hero" :post/content "y" :post/author "u1"
                       :post/featured? true :post/pages ["Home"]})
  (:post/featured? (get p {:post/id 2})) ;=> false
  (:post/featured? (get p {:post/id 3})) ;=> true

  ;; Different page — no interference
  (coll/mutate! p nil {:post/title "About" :post/content "z" :post/author "u1"
                       :post/featured? true :post/pages ["About"]})
  (:post/featured? (get p {:post/id 3})) ;=> true
  (:post/featured? (get p {:post/id 4})) ;=> true

  ;; Delete
  (coll/mutate! p {:post/id 1} nil) ;=> true

  (release-conn! conn))

^:rct/test
(comment
  ;; --- Users collection ---
  (def conn (create-conn!))
  (create-user! conn #:user{:id "u1" :email "a@b.com" :name "Alice" :picture ""})
  (create-user! conn #:user{:id "u2" :email "b@b.com" :name "Bob" :picture ""})
  (def u (users conn))

  (count (seq u)) ;=> 2
  (:user/name (get u {:user/id "u1"})) ;=> "Alice"

  (release-conn! conn))

^:rct/test
(comment
  ;; --- User-roles collection ---
  (def conn (create-conn!))
  (create-user! conn #:user{:id "u1" :email "a@b.com" :name "A" :picture ""})

  (:role/name (coll/mutate! (user-roles conn "u1") nil {:role/name :member})) ;=> :member
  (:role/name (get (user-roles conn "u1") {:role/name :member})) ;=> :member
  (coll/mutate! (user-roles conn "u1") {:role/name :member} nil) ;=> true
  (coll/mutate! (user-roles conn "u1") {:role/name :admin} nil) ;=> false

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

  (let [p (posts conn)]
    ;; Home page content (featured = hero post)
    (coll/mutate! p nil {:post/title "Welcome to Flybot"
                         :post/author "sample-alice"
                         :post/pages ["Home"]
                         :post/tags ["clojure"]
                         :post/featured? true
                         :post/content "# Building the Future of Software

At Flybot, we're passionate about creating elegant solutions using **functional programming** and **data-driven design**.

## What We Do

- **Custom Software Development** - Tailored solutions for your business needs
- **Technical Consulting** - Expert guidance on architecture and best practices
- **Training & Workshops** - Level up your team's Clojure skills

*Innovation through simplicity.*"})

    ;; Additional Home posts for slideshow
    (coll/mutate! p nil {:post/title "Latest News: Q1 2026 Update"
                         :post/author "sample-bob"
                         :post/pages ["Home"]
                         :post/tags ["news"]
                         :post/content "We're excited to announce **several new client partnerships** and the release of our open-source [pull-pattern library](https://github.com/flybot-sg/pull-pattern)!

## Highlights

- Signed contracts with *3 major enterprises*
- Open-sourced our core pattern matching library
- Expanded the team with 2 new engineers"})

    (coll/mutate! p nil {:post/title "Featured Project: Data Pipeline"
                         :post/author "sample-alice"
                         :post/pages ["Home"]
                         :post/tags ["projects" "tech" "clojure"]
                         :post/content "Check out our latest case study on building **high-performance data pipelines** with Clojure and Kafka.

## Architecture

```clojure
(defn process-event [event]
  (-> event
      validate-schema
      transform-data
      persist!))
```

The pipeline handles *10,000 events/second* with sub-millisecond latency."})

    ;; About page content (featured = hero post)
    (coll/mutate! p nil {:post/title "About Flybot"
                         :post/author "sample-alice"
                         :post/pages ["About"]
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

    (coll/mutate! p nil {:post/title "Our Tech Stack"
                         :post/author "sample-bob"
                         :post/pages ["About"]
                         :post/tags ["tech" "clojure"]
                         :post/content "We work primarily with **Clojure/ClojureScript**, Datomic, and modern cloud infrastructure.

## Core Technologies

| Layer | Technology |
|-------|------------|
| Backend | Clojure, Ring, Reitit |
| Frontend | ClojureScript, Replicant |
| Database | Datomic, Datahike |
| Infra | AWS, Docker, Terraform |

Our tooling philosophy emphasizes *composability* and data-driven configuration."})

    ;; Apply page content (featured = hero post)
    (coll/mutate! p nil {:post/title "Join Our Team"
                         :post/author "sample-bob"
                         :post/pages ["Apply"]
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

    (coll/mutate! p nil {:post/title "Internship Program"
                         :post/author "sample-alice"
                         :post/pages ["Apply"]
                         :post/tags ["internship"]
                         :post/content "Our **3-month internship program** is designed for students and early-career developers eager to learn functional programming.

## What You'll Learn

1. REPL-driven development workflow
2. Functional programming fundamentals
3. Building production-grade APIs
4. Testing with `clojure.test` and RCT

Interns work on *real projects* alongside senior engineers. [Apply now](mailto:careers@flybot.sg)!"})

    ;; Regular blog posts
    (coll/mutate! p nil {:post/title "Understanding Pull Patterns"
                         :post/author "sample-alice"
                         :post/tags ["clojure" "patterns"]
                         :post/content "Pull patterns let you **declaratively specify** what data you want from nested data structures.

## The Problem

Instead of writing imperative traversal code:

```clojure
(get-in data [:user :address :city])
```

You describe the *shape of data* you need:

```clojure
'{:user {:address {:city ?city}}}
```

> Let the pattern engine do the work."})

    (coll/mutate! p nil {:post/title "Building APIs with Lazy Data"
                         :post/author "sample-bob"
                         :post/tags ["clojure" "api"]
                         :post/content "The key insight is that your API is just a **lazy data structure**.

## ILookup Protocol

By implementing `ILookup`, you can create collections that fetch data *on-demand*:

```clojure
(defrecord LazyCollection [fetch-fn]
  ILookup
  (valAt [_ k] (fetch-fn k)))
```

This makes your API both flexible and efficient."})

    (coll/mutate! p nil {:post/title "REPL-Driven Development Tips"
                         :post/author "sample-alice"
                         :post/tags ["clojure" "workflow"]
                         :post/content "Rich comment blocks (`^:rct/test`) combine **documentation**, **examples**, and **tests** in one place.

## Example

```clojure
^:rct/test
(comment
  (add 1 2) ;=> 3
  (add -1 1) ;=> 0)
```

They're evaluated during development but *ignored in production*."})

    (coll/mutate! p nil {:post/title "Clojure中的函数式编程"
                         :post/author "sample-zhang"
                         :post/tags ["clojure" "functional"]
                         :post/content "Functional programming in Clojure emphasizes **immutability** and **pure functions**.

## 核心概念

- 不可变数据结构 (*Immutable data*)
- 纯函数 (*Pure functions*)
- 高阶函数 (*Higher-order functions*)

This approach leads to code that is easier to test, reason about, and parallelize."})))
