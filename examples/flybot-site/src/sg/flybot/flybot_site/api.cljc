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

(def post-schema
  "Schema for a single post."
  {:post/id :number
   :post/title :string
   :post/content :string
   :post/author :string
   :post/tags [:string]
   :post/created-at :any
   :post/updated-at :any})

(def post-query
  "Query schema for post lookup (indexed fields)."
  [:or
   {:post/id :number}
   {:post/author :string}])

(def version-schema
  "Schema for a historical version of a post."
  (merge post-schema
         {:version/tx :number
          :version/timestamp :any}))

(def schema
  "API schema - noun-only, single source of truth.

   :posts supports both sequential and indexed access:
   - As sequence: (seq posts) → all posts
   - As lookup: (get posts {:id 3}) → post by query
   :posts/history returns historical versions of a post:
   - {:posts/history {{:post/id 1} ?versions}}"
  {:posts [:union [post-schema] {post-query post-schema}]
   :posts/history {post-query [version-schema]}})

(def viewer-schema
  "Read-only schema - list and lookup access only, no mutations.

   Key difference from `schema`:
   - Uses [post-schema] instead of [:union ...] to disallow mutation syntax
   - Used for anonymous users and non-owner authenticated users"
  {:posts [post-schema]
   :posts/history {post-query [version-schema]}})

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
    (count (seq (:posts api)))) ;=> 3

  ;; READ: lookup by id via ILookup
  (let [api (make-api conn)]
    (:post/title (get (:posts api) {:post/id 1}))) ;=> "Welcome to My Blog"

  ;; CREATE: mutate! with nil query (content has frontmatter)
  (let [api (make-api conn)
        created (coll/mutate! (:posts api) nil {:post/title "New" :post/content "---\nauthor: Test\ntags:\n  - demo\n---\n\nPost body"})]
    (:post/title created)) ;=> "New"

  ;; READ: verify created post and frontmatter extraction
  (let [api (make-api conn)
        post (get (:posts api) {:post/id 4})]
    [(:post/title post) (:post/author post) (:post/tags post)])
  ;=> ["New" "Test" ["demo"]]

  ;; UPDATE: mutate! with query and data
  (let [api (make-api conn)
        updated (coll/mutate! (:posts api) {:post/id 4} {:post/title "Updated"})]
    (:post/title updated)) ;=> "Updated"

  ;; DELETE: mutate! with query and nil
  (let [api (make-api conn)]
    (coll/mutate! (:posts api) {:post/id 4} nil)) ;=> true

  ;; Verify deleted
  (let [api (make-api conn)]
    (get (:posts api) {:post/id 4})) ;=> nil

  ;; Pattern matching still works for LIST
  (let [api (make-api conn)]
    (count ((p/match-fn {:posts ?posts} ?posts {:schema schema}) api))) ;=> 3

  ;; Cleanup
  (db/release-conn! conn))
