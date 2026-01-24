(ns sg.flybot.blog.db
  "In-memory blog database.

   Implements DataSource for CRUD operations.
   Use `posts` to get a collection for pattern-based access.

   ## Usage

   ```clojure
   (def p (posts db))
   (seq p)                        ; list all
   (get p {:id 3})                ; fetch by id
   (coll/mutate! p nil data)      ; create
   (coll/mutate! p {:id 3} data)  ; update
   (coll/mutate! p {:id 3} nil)   ; delete
   ```"
  (:require
   [sg.flybot.pullable.collection :as coll]))

;;=============================================================================
;; State
;;=============================================================================

(defonce db (atom {}))
(defonce ^:private id-counter (atom 0))

(defn reset-db!
  "Reset database to empty state."
  ([] (reset-db! db))
  ([db-atom]
   (reset! db-atom {})
   (reset! id-counter 0)))

;;=============================================================================
;; DataSource Implementation
;;=============================================================================

(defn- next-id []
  (swap! id-counter inc))

(defn- now []
  #?(:clj (java.util.Date.)
     :cljs (js/Date.)))

(defrecord PostsDataSource [db-atom]
  coll/DataSource
  (fetch [_ query]
    (cond
      ;; By ID
      (contains? query :id)
      (get @db-atom (:id query))

      ;; By other fields (scan)
      :else
      (first (filter #(every? (fn [[k v]] (= (get % k) v)) query)
                     (vals @db-atom)))))

  (list-all [_]
    (sort-by :created-at #(compare %2 %1) (vals @db-atom)))

  (create! [_ {:keys [title content author tags]}]
    (let [id (next-id)
          post {:id id
                :title title
                :content content
                :author author
                :tags (or tags [])
                :created-at (now)
                :updated-at (now)}]
      (swap! db-atom assoc id post)
      post))

  (update! [this query data]
    (when-let [post (coll/fetch this query)]
      (let [updated (merge post data {:updated-at (now)})]
        (swap! db-atom assoc (:id post) updated)
        updated)))

  (delete! [this query]
    (if-let [post (coll/fetch this query)]
      (do (swap! db-atom dissoc (:id post)) true)
      false)))

;;=============================================================================
;; Collection Constructor
;;=============================================================================

(defn posts
  "Create a posts collection for the given database.

   Options:
   - :indexes - Set of indexed field sets (default #{#{:id}})

   Returns a collection implementing ILookup, Seqable, Mutable, Wireable."
  ([db-atom] (posts db-atom {}))
  ([db-atom opts]
   (coll/collection (->PostsDataSource db-atom) opts)))

^:rct/test
(comment
  ;; Setup
  (reset-db!)
  (def p (posts db))

  ;; CREATE
  (def created (coll/mutate! p nil {:title "Test" :content "Hello" :author "Me"}))
  (:id created) ;=> 1
  (:title created) ;=> "Test"

  ;; READ via ILookup
  (:title (get p {:id 1})) ;=> "Test"
  (get p {:id 999}) ;=> nil

  ;; LIST via seq
  (coll/mutate! p nil {:title "Second" :content "Two" :author "You"})
  (count (seq p)) ;=> 2

  ;; UPDATE
  (:title (coll/mutate! p {:id 1} {:title "Updated"})) ;=> "Updated"
  (:title (get p {:id 1})) ;=> "Updated"

  ;; DELETE
  (coll/mutate! p {:id 1} nil) ;=> true
  (get p {:id 1}) ;=> nil
  (count p) ;=> 1

  ;; Query without index throws
  (ex-data
   (try
     (get p {:author "You"})
     (catch Exception e e))) ;=>> {:query {:author "You"}}

  ;; Query with index works
  (def p2 (posts db {:indexes #{#{:id} #{:author}}}))
  (:title (get p2 {:author "You"})) ;=> "Second"

  ;; Cleanup
  (reset-db!))

;;=============================================================================
;; Seed Data
;;=============================================================================

(defn seed!
  "Seed database with sample data."
  ([] (seed! db))
  ([db-atom]
   (reset-db! db-atom)
   (let [ds (->PostsDataSource db-atom)]
     (coll/create! ds {:title "Welcome to My Blog"
                       :content "This is my first post using the pull-based API!"
                       :author "Alice"
                       :tags ["welcome" "meta"]})
     (coll/create! ds {:title "Understanding Pull Patterns"
                       :content "Pull patterns let you declaratively specify what data you want..."
                       :author "Alice"
                       :tags ["clojure" "patterns"]})
     (coll/create! ds {:title "Building APIs with Lazy Data"
                       :content "The key insight is that your API is just a lazy data structure..."
                       :author "Bob"
                       :tags ["clojure" "api"]}))))
