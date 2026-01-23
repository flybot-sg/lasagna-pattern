(ns sg.flybot.blog.db
  "In-memory blog database.

   All functions take a db atom as first parameter.
   A default global db is provided for simple usage.")

;;=============================================================================
;; Default Global State
;;=============================================================================

(defonce db (atom {}))
(defonce ^:private id-counter (atom 0))

(defn reset-db!
  "Reset a database to empty state."
  ([] (reset-db! db))
  ([db-atom]
   (reset! db-atom {})
   (reset! id-counter 0)))

;;=============================================================================
;; CRUD Operations
;;=============================================================================

(defn- next-id []
  (swap! id-counter inc))

(defn create-post!
  "Create a new post. Returns the created post with :id."
  ([data] (create-post! db data))
  ([db-atom {:keys [title content author tags]}]
   (let [id (next-id)
         now #?(:clj (java.util.Date.) :cljs (js/Date.))
         post {:id id
               :title title
               :content content
               :author author
               :tags (or tags [])
               :created-at now
               :updated-at now}]
     (swap! db-atom assoc id post)
     post)))

(defn get-post
  "Get a post by ID."
  ([id] (get-post db id))
  ([db-atom id]
   (get @db-atom id)))

(defn update-post!
  "Update a post. Returns updated post or nil if not found."
  ([id updates] (update-post! db id updates))
  ([db-atom id updates]
   (when (contains? @db-atom id)
     (let [now #?(:clj (java.util.Date.) :cljs (js/Date.))]
       (swap! db-atom update id merge updates {:updated-at now})
       (get @db-atom id)))))

(defn delete-post!
  "Delete a post. Returns true if deleted, false if not found."
  ([id] (delete-post! db id))
  ([db-atom id]
   (if (contains? @db-atom id)
     (do (swap! db-atom dissoc id) true)
     false)))

(defn list-posts
  "List all posts, optionally filtered."
  ([] (list-posts db {}))
  ([opts-or-db]
   (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) opts-or-db)
     (list-posts opts-or-db {})
     (list-posts db opts-or-db)))
  ([db-atom {:keys [tag author limit offset]}]
   (cond->> (sort-by :created-at #(compare %2 %1) (vals @db-atom))
     tag (filter #(some #{tag} (:tags %)))
     author (filter #(= author (:author %)))
     offset (drop offset)
     limit (take limit))))

(defn count-posts
  "Count posts matching criteria."
  ([] (count-posts db {}))
  ([opts-or-db]
   (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) opts-or-db)
     (count-posts opts-or-db {})
     (count-posts db opts-or-db)))
  ([db-atom opts]
   (count (list-posts db-atom opts))))

;;=============================================================================
;; Seed Data
;;=============================================================================

(defn seed!
  "Seed database with sample data."
  ([] (seed! db))
  ([db-atom]
   (reset-db! db-atom)
   (create-post! db-atom {:title "Welcome to My Blog"
                          :content "This is my first post using the pull-based API!"
                          :author "Alice"
                          :tags ["welcome" "meta"]})
   (create-post! db-atom {:title "Understanding Pull Patterns"
                          :content "Pull patterns let you declaratively specify what data you want..."
                          :author "Alice"
                          :tags ["clojure" "patterns"]})
   (create-post! db-atom {:title "Building APIs with Lazy Data"
                          :content "The key insight is that your API is just a lazy data structure..."
                          :author "Bob"
                          :tags ["clojure" "api"]})))

^:rct/test
(comment
  ;; Setup
  (reset-db!)
  (count-posts) ;=> 0

  ;; Create
  (def p1 (create-post! {:title "Test" :content "Hello" :author "Me"}))
  (:id p1) ;=> 1
  (:title p1) ;=> "Test"

  ;; Read
  (:title (get-post 1)) ;=> "Test"
  (get-post 999) ;=> nil

  ;; Update
  (:title (update-post! 1 {:title "Updated"})) ;=> "Updated"
  (update-post! 999 {:title "Nope"}) ;=> nil

  ;; List
  (create-post! {:title "Second" :content "Two" :author "You" :tags ["a"]})
  (count (list-posts)) ;=> 2
  (count (list-posts {:author "Me"})) ;=> 1
  (count (list-posts {:tag "a"})) ;=> 1

  ;; Delete
  (delete-post! 1) ;=> true
  (delete-post! 1) ;=> false
  (count-posts) ;=> 1

  ;; Cleanup
  (reset-db!))
