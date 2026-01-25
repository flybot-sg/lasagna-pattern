(ns sg.flybot.flybot-site.api
  "Blog API - noun-only schema with ILookup-based CRUD.

   The schema is purely nouns - no verbs like create/update/delete.
   CRUD operations are expressed through pattern syntax on collections.

   ## Schema
   {:posts (union [post-schema] {post-query post-schema})}

   ## Access Patterns
   | Pattern                              | Operation |
   |--------------------------------------|-----------|
   | {:posts ?all}                        | LIST      |
   | {:posts {{:id 3} ?post}}             | READ      |
   | {:posts {nil {:title ...}}}          | CREATE    |
   | {:posts {{:id 3} {:title ...}}}      | UPDATE    |
   | {:posts {{:id 3} nil}}               | DELETE    |

   See remote/doc/DESIGN.md for full CRUD protocol specification."
  (:require
   [sg.flybot.flybot-site.db :as db]
   [sg.flybot.pullable.collection :as coll]
   [robertluo.fun-map :refer [fnk life-cycle-map]]))

;;=============================================================================
;; Schema
;;=============================================================================

(def post-fields
  "Field documentation for post schema."
  {:post/id         {:doc "Unique identifier" :example 1}
   :post/title      {:doc "Post title"}
   :post/content    {:doc "Markdown content with optional YAML frontmatter"}
   :post/author     {:doc "Author name (extracted from frontmatter)"}
   :post/tags       {:doc "List of tags (extracted from frontmatter)" :example ["clojure" "web"]}
   :post/created-at {:doc "Creation timestamp"}
   :post/updated-at {:doc "Last update timestamp"}})

(def post-schema
  "Schema for a single post."
  (with-meta
    {:post/id :number
     :post/title :string
     :post/content :string
     :post/author :string
     :post/tags [:string]
     :post/created-at :any
     :post/updated-at :any}
    {:doc "Blog post" :fields post-fields}))

(def post-query
  "Query schema for post lookup (indexed fields)."
  [:or
   {:post/id :number}
   {:post/author :string}])

(def version-schema
  "Schema for a historical version of a post."
  (with-meta
    (merge post-schema
           {:version/tx :number
            :version/timestamp :any})
    {:doc "Historical version of a post"
     :fields (merge post-fields
                    {:version/tx        {:doc "Transaction ID"}
                     :version/timestamp {:doc "When this version was created"}})}))

(def schema
  "API schema - noun-only, single source of truth."
  (with-meta
    {:posts [:union [post-schema] {post-query post-schema}]
     :posts/history {post-query [version-schema]}}
    {:doc "Flybot Blog API"
     :fields {:posts         {:doc "Blog posts collection"}
              :posts/history {:doc "Post version history"}}
     :operations {:posts {:list   "Returns all posts"
                          :get    "Lookup by {:post/id n} or {:post/author s}"
                          :create "Provide {:post/title :post/content}"
                          :update "Partial updates supported"
                          :delete "Returns true on success"}}}))

(def viewer-schema
  "Read-only schema for anonymous and non-owner users."
  (with-meta
    {:posts :any
     :posts/history {post-query [version-schema]}}
    {:doc "Flybot Blog API (read-only)"
     :fields {:posts         {:doc "Blog posts (read-only)"}
              :posts/history {:doc "Post version history"}}}))

;;=============================================================================
;; API Builder
;;=============================================================================

(defn make-api
  "Build API with Posts collection.

   Arguments:
   - conn - Datahike connection
   - opts - Options map:
     - :indexes - Set of indexed field sets for queries (default #{#{:id}})

   Returns a map with:
   - :posts - Posts collection supporting ILookup and Seqable
   - :posts/history - History lookup supporting ILookup

   Usage:
     (def api (make-api conn))
     (seq (:posts api))                   ; LIST all posts
     (get (:posts api) {:id 3})           ; READ by id
     (coll/mutate! (:posts api) nil {...})      ; CREATE
     (coll/mutate! (:posts api) {:id 3} {...})  ; UPDATE
     (coll/mutate! (:posts api) {:id 3} nil)    ; DELETE
     (get (:posts/history api) {:post/id 3})    ; HISTORY"
  ([conn] (make-api conn {}))
  ([conn {:keys [indexes] :or {indexes #{#{:post/id}}}}]
   (life-cycle-map
    {:posts (fnk [] (db/posts conn {:indexes indexes}))
     :posts/history (fnk [] (db/post-history-lookup conn))})))

^:rct/test
(comment
  (require '[sg.flybot.pullable :as p])

  ;; Setup
  (def conn (db/create-conn!))
  (db/seed! conn)

  ;; LIST: get all posts via seq
  (let [api (make-api conn)]
    (count (seq (:posts api)))) ;=> 10

  ;; READ: lookup by id via ILookup
  (let [api (make-api conn)]
    (:post/title (get (:posts api) {:post/id 1}))) ;=> "Welcome to Flybot"

  ;; CREATE: mutate! with nil query (content has frontmatter)
  (let [api (make-api conn)
        created (coll/mutate! (:posts api) nil {:post/title "New" :post/content "---\nauthor: Test\ntags:\n  - demo\n---\n\nPost body"})]
    (:post/title created)) ;=> "New"

  ;; READ: verify created post and frontmatter extraction
  (let [api (make-api conn)
        post (get (:posts api) {:post/id 11})]
    [(:post/title post) (:post/author post) (:post/tags post)])
  ;=> ["New" "Test" ["demo"]]

  ;; UPDATE: mutate! with query and data
  (let [api (make-api conn)
        updated (coll/mutate! (:posts api) {:post/id 11} {:post/title "Updated"})]
    (:post/title updated)) ;=> "Updated"

  ;; DELETE: mutate! with query and nil
  (let [api (make-api conn)]
    (coll/mutate! (:posts api) {:post/id 11} nil)) ;=> true

  ;; Verify deleted
  (let [api (make-api conn)]
    (get (:posts api) {:post/id 11})) ;=> nil

  ;; Pattern matching still works for LIST
  (let [api (make-api conn)]
    (count ((p/match-fn {:posts ?posts} ?posts {:schema schema}) api))) ;=> 10

  ;; Cleanup
  (db/release-conn! conn))
