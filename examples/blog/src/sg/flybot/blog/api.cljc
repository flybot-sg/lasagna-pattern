(ns sg.flybot.blog.api
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
   [sg.flybot.blog.db :as db]
   [sg.flybot.pullable.remote :as remote]
   [robertluo.fun-map :refer [fnk life-cycle-map]]))

;;=============================================================================
;; Schema
;;=============================================================================

(def post-schema
  "Schema for a single post."
  {:id :number
   :title :string
   :content :string
   :author :string
   :tags [:string]
   :created-at :any
   :updated-at :any})

(def post-query
  "Query schema for post lookup (indexed fields)."
  [:or
   {:id :number}
   {:author :string}])

(def schema
  "API schema - noun-only, single source of truth.

   :posts supports both sequential and indexed access:
   - As sequence: (seq posts) → all posts
   - As lookup: (get posts {:id 3}) → post by query"
  {:posts [:union [post-schema] {post-query post-schema}]})

;;=============================================================================
;; API Builder
;;=============================================================================

(defn make-api
  "Build API with Posts collection.

   Arguments:
   - db-atom - Database atom (use db/db for global, or inject your own)
   - opts - Options map:
     - :indexes - Set of indexed field sets for queries (default #{#{:id}})

   Returns a map with:
   - :posts - Posts collection supporting ILookup and Seqable

   Usage:
     (def api (make-api db/db))
     (seq (:posts api))                   ; LIST all posts
     (get (:posts api) {:id 3})           ; READ by id
     (remote/mutate! (:posts api) nil {...})      ; CREATE
     (remote/mutate! (:posts api) {:id 3} {...})  ; UPDATE
     (remote/mutate! (:posts api) {:id 3} nil)    ; DELETE"
  ([db-atom] (make-api db-atom {}))
  ([db-atom {:keys [indexes] :or {indexes #{#{:id}}}}]
   (life-cycle-map
    {:posts (fnk [] (db/posts db-atom {:indexes indexes}))})))

^:rct/test
(comment
  (require '[sg.flybot.pullable :as p])

  ;; Setup
  (db/seed!)

  ;; LIST: get all posts via seq
  (let [api (make-api db/db)]
    (count (seq (:posts api)))) ;=> 3

  ;; READ: lookup by id via ILookup
  (let [api (make-api db/db)]
    (:title (get (:posts api) {:id 1}))) ;=> "Welcome to My Blog"

  ;; CREATE: mutate! with nil query
  (let [api (make-api db/db)
        created (remote/mutate! (:posts api) nil {:title "New" :content "Post" :author "Test"})]
    (:title created)) ;=> "New"

  ;; READ: verify created post
  (let [api (make-api db/db)]
    (:title (get (:posts api) {:id 4}))) ;=> "New"

  ;; UPDATE: mutate! with query and data
  (let [api (make-api db/db)
        updated (remote/mutate! (:posts api) {:id 4} {:title "Updated"})]
    (:title updated)) ;=> "Updated"

  ;; DELETE: mutate! with query and nil
  (let [api (make-api db/db)]
    (remote/mutate! (:posts api) {:id 4} nil)) ;=> true

  ;; Verify deleted
  (let [api (make-api db/db)]
    (get (:posts api) {:id 4})) ;=> nil

  ;; Pattern matching still works for LIST
  (let [api (make-api db/db)]
    (count ((p/match-fn {:posts ?posts} ?posts {:schema schema}) api))) ;=> 3

  ;; Cleanup
  (db/reset-db!))
