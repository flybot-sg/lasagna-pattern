(ns sg.flybot.blog.api
  "Blog API - lazy data structure with schema.

   The API is a lazy map where keys compute on demand.
   Pattern matching drives what gets evaluated.

   Usage:
     ;; With injected db (system integration)
     (make-api my-db {:params {:post-id 1}})

     ;; With global db (simple usage)
     (make-api db/db {:params {:post-id 1}})"
  (:require
   [sg.flybot.blog.db :as db]
   [robertluo.fun-map :refer [fnk life-cycle-map]]))

;;=============================================================================
;; Schema
;;=============================================================================

(def post-schema
  {:id :number
   :title :string
   :content :string
   :author :string
   :tags [:string]
   :created-at :any
   :updated-at :any})

(def schema
  "API schema - defines what patterns can request."
  {:post post-schema
   :posts [post-schema]
   :post-count :number
   ;; Mutations look the same as queries
   :create-post post-schema
   :update-post post-schema
   :delete-post :boolean})

;;=============================================================================
;; API Builder
;;=============================================================================

(defn make-api
  "Build lazy API with database and request context.

   Arguments:
   - db-atom - Database atom (use db/db for global, or inject your own)
   - context - Map with :params for query parameters

   The API is a life-cycle-map where:
   - Keys are computed only when accessed
   - Dependencies between keys are tracked
   - Schema metadata controls what's accessible

   Pattern input fields (literals) drive mutations:
   - {:create-post {:title \"Hello\" :content \"World\" :author \"Me\"} ...}
     ^-- this creates a post because :create-post depends on input

   Query fields (variables) extract data:
   - {:post {:id 1 :title ?title}}
     ^-- this reads post 1 and binds title to ?title"
  [db-atom {:keys [params] :as _context}]
  (life-cycle-map
   ;; ---- Queries ----

   {:post
    (fnk []
         (when-let [id (:post-id params)]
           (db/get-post db-atom id)))

    :posts
    (fnk []
         (db/list-posts db-atom params))

    :post-count
    (fnk []
         (db/count-posts db-atom params))

    ;; ---- Mutations ----

    :create-post
    (fnk []
         (when-let [data (:create-post params)]
           (db/create-post! db-atom data)))

    :update-post
    (fnk []
         (when-let [{:keys [id] :as data} (:update-post params)]
           (db/update-post! db-atom id (dissoc data :id))))

    :delete-post
    (fnk []
         (when-let [id (:post-id params)]
           (db/delete-post! db-atom id)))}))

^:rct/test
(comment
  (require '[sg.flybot.pullable :as p])

  ;; Setup
  (db/seed!)

  ;; Query: list posts (bind whole sequence)
  (let [api (make-api db/db {})]
    (count ((p/match-fn {:posts ?posts} ?posts {:schema schema}) api))) ;=> 3

  ;; Query: single post
  (let [api (make-api db/db {:params {:post-id 1}})]
    ((p/match-fn {:post {:id ?id :title ?t}} ?t) api)) ;=> "Welcome to My Blog"

  ;; Mutation: create post
  (let [api (make-api db/db {:params {:create-post {:title "New" :content "Post" :author "Test"}}})]
    ((p/match-fn {:create-post {:id ?id :title ?t}} ?t) api)) ;=> "New"

  ;; Mutation: delete
  (let [id 4 ;; the one we just created
        api (make-api db/db {:params {:post-id id}})]
    ((p/match-fn {:delete-post ?deleted} ?deleted) api)) ;=> true

  ;; Cleanup
  (db/reset-db!))
