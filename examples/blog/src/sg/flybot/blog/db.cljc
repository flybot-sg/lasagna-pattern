(ns sg.flybot.blog.db
  "In-memory blog database with ILookup-based collection access.

   All functions take a db atom as first parameter.
   A default global db is provided for simple usage.

   ## CRUD via Posts Collection

   The `posts` function returns a Posts collection that supports:
   - Sequential access: `(seq posts)` → all posts
   - Indexed lookup: `(get posts {:id 3})` → post by id
   - Mutations: `(mutate! posts query value)` → create/update/delete

   Query keys are maps that specify field constraints:
   - `{:id 3}` → find post with id=3
   - `{:author \"Alice\"}` → find posts by author (if indexed)

   Mutation semantics:
   - `(mutate! posts nil data)` → CREATE (nil query = insert)
   - `(mutate! posts {:id 3} data)` → UPDATE (query + data)
   - `(mutate! posts {:id 3} nil)` → DELETE (query + nil value)"
  (:require
   [sg.flybot.pullable.remote :as remote]))

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
;; Posts Collection (ILookup-based CRUD)
;;=============================================================================

(defprotocol Mutable
  "Protocol for collections that support CRUD mutations."
  (mutate! [coll query value]
    "Perform mutation based on query and value:
     - (mutate! coll nil data) → CREATE
     - (mutate! coll query data) → UPDATE
     - (mutate! coll query nil) → DELETE"))

(deftype Posts [db-atom indexes]
  #?@(:clj
      [clojure.lang.ILookup
       (valAt [_ query]
              (.valAt _ query nil))
       (valAt [_ query not-found]
              (cond
           ;; {:id 3} → lookup by primary key
                (and (map? query) (contains? query :id))
                (or (get @db-atom (:id query)) not-found)

           ;; Other indexed queries - check if index exists
                (map? query)
                (let [query-keys (set (keys query))]
                  (if (some #(= query-keys %) indexes)
               ;; Index exists - perform lookup
                    (first (filter #(every? (fn [[k v]] (= (get % k) v)) query)
                                   (vals @db-atom)))
               ;; No index - throw
                    (throw (ex-info "No index for query" {:query query
                                                          :available-indexes indexes}))))

                :else not-found))

       clojure.lang.Seqable
       (seq [_]
            (seq (sort-by :created-at #(compare %2 %1) (vals @db-atom))))

       clojure.lang.Counted
       (count [_]
              (count @db-atom))]

      :cljs
      [ILookup
       (-lookup [_ query]
                (-lookup _ query nil))
       (-lookup [_ query not-found]
                (cond
                  (and (map? query) (contains? query :id))
                  (or (get @db-atom (:id query)) not-found)

                  (map? query)
                  (let [query-keys (set (keys query))]
                    (if (some #(= query-keys %) indexes)
                      (first (filter #(every? (fn [[k v]] (= (get % k) v)) query)
                                     (vals @db-atom)))
                      (throw (ex-info "No index for query" {:query query
                                                            :available-indexes indexes}))))

                  :else not-found))

       ISeqable
       (-seq [_]
             (seq (sort-by :created-at #(compare %2 %1) (vals @db-atom))))

       ICounted
       (-count [_]
               (count @db-atom))]))

(extend-type Posts
  Mutable
  (mutate! [coll query value]
    (let [db-atom (.-db-atom coll)]
      (cond
        ;; CREATE: nil query + data
        (and (nil? query) (some? value))
        (create-post! db-atom value)

        ;; DELETE: query + nil value
        (and (some? query) (nil? value))
        (when-let [post (get coll query)]
          (delete-post! db-atom (:id post)))

        ;; UPDATE: query + data
        (and (some? query) (some? value))
        (when-let [post (get coll query)]
          (update-post! db-atom (:id post) value))

        :else
        (throw (ex-info "Invalid mutation" {:query query :value value})))))

  remote/Wireable
  (->wire [coll]
    (vec (seq coll))))

(defn posts
  "Create a Posts collection for the given database.

   Options:
   - :indexes - set of indexed field sets, e.g. #{#{:id} #{:author}}
                Defaults to #{#{:id}} (primary key only)

   Usage:
     (def p (posts db))
     (seq p)                    ; list all
     (get p {:id 3})            ; lookup by id
     (mutate! p nil {:title ..}) ; create
     (mutate! p {:id 3} {:title ..}) ; update
     (mutate! p {:id 3} nil)    ; delete"
  ([db-atom] (posts db-atom {}))
  ([db-atom {:keys [indexes] :or {indexes #{#{:id}}}}]
   (->Posts db-atom indexes)))

^:rct/test
(comment
  ;; Setup
  (reset-db!)
  (def p (posts db))

  ;; CREATE via mutate!
  (def created (mutate! p nil {:title "Test" :content "Hello" :author "Me"}))
  (:id created) ;=> 1
  (:title created) ;=> "Test"

  ;; READ via ILookup
  (:title (get p {:id 1})) ;=> "Test"
  (get p {:id 999}) ;=> nil

  ;; LIST via seq
  (mutate! p nil {:title "Second" :content "Two" :author "You"})
  (count (seq p)) ;=> 2

  ;; UPDATE via mutate!
  (:title (mutate! p {:id 1} {:title "Updated"})) ;=> "Updated"
  (:title (get p {:id 1})) ;=> "Updated"

  ;; DELETE via mutate!
  (mutate! p {:id 1} nil) ;=> true
  (get p {:id 1}) ;=> nil
  (count p) ;=> 1

  ;; Query without index throws (use ex-data to check)
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
