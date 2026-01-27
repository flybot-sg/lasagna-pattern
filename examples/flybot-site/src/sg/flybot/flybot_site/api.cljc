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
   [sg.flybot.pullable.malli]
   [malli.core :as m]
   [malli.util :as mu]
   [robertluo.fun-map :refer [fnk life-cycle-map]]))

;;=============================================================================
;; Schema
;;=============================================================================

(def post-schema
  "Schema for a single post. Uses inline Malli properties for documentation."
  (m/schema
   [:map {:doc "Blog post"}
    [:post/id {:doc "Unique identifier" :example 1} :int]
    [:post/title {:doc "Post title"} :string]
    [:post/content {:doc "Markdown content with optional YAML frontmatter"} :string]
    [:post/author {:doc "Author name (extracted from frontmatter)"} :string]
    [:post/tags {:doc "List of tags (extracted from frontmatter)" :example ["clojure" "web"]} [:vector :string]]
    [:post/created-at {:doc "Creation timestamp"} :any]
    [:post/updated-at {:doc "Last update timestamp"} :any]]))

(def post-query
  "Query schema for post lookup (indexed fields)."
  (m/schema
   [:or
    [:map [:post/id :int]]
    [:map [:post/author :string]]]))

(def version-schema
  "Schema for a historical version of a post. Extends post-schema with version fields."
  (mu/merge
   post-schema
   (m/schema
    [:map {:doc "Historical version of a post"}
     [:version/tx {:doc "Transaction ID"} :int]
     [:version/timestamp {:doc "When this version was created"} :any]])))

(def schema
  "API schema - noun-only, single source of truth.
   Top-level is a plain map so auth module can assoc :me key."
  (with-meta
    {:posts (m/schema [:or [:vector post-schema] [:map-of post-query post-schema]])
     :posts/history (m/schema [:map-of post-query [:vector version-schema]])}
    {:doc "Flybot Blog API"
     :fields {:posts         {:doc "Blog posts collection"}
              :posts/history {:doc "Post version history"}}
     :operations {:posts {:list   "Returns all posts"
                          :get    "Lookup by {:post/id n} or {:post/author s}"
                          :create "Provide {:post/title :post/content}"
                          :update "Partial updates supported"
                          :delete "Returns true on success"}}}))

(def viewer-schema
  "Read-only schema for anonymous and non-owner users.
   Top-level is a plain map so auth module can assoc :me key."
  (with-meta
    {:posts (m/schema [:vector {:ilookup true} post-schema])
     :posts/history (m/schema [:map-of post-query [:vector version-schema]])}
    {:doc "Flybot Blog API (read-only)"
     :fields {:posts         {:doc "Blog posts (read-only)"}
              :posts/history {:doc "Post version history"}}}))

^:rct/test
(comment
  ;; All schemas must be valid Malli schemas
  (m/schema? post-schema) ;=> true
  (m/schema? post-query) ;=> true
  (m/schema? version-schema) ;=> true
  (m/schema? (:posts schema)) ;=> true
  (m/schema? (:posts/history schema)) ;=> true
  (m/schema? (:posts viewer-schema)) ;=> true
  (m/schema? (:posts/history viewer-schema)) ;=> true

  ;; version-schema extends post-schema with version fields
  (contains? (set (map first (m/children version-schema))) :version/tx) ;=> true
  (contains? (set (map first (m/children version-schema))) :post/id)) ;=> true)

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
