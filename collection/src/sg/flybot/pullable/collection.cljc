(ns sg.flybot.pullable.collection
  "CRUD collection abstraction with transactional support.

   Provides a generic Collection type that wraps a DataSource backend
   and implements ILookup, Seqable, Counted for pattern-based access.

   ## Basic Usage

   ```clojure
   (def src (atom-source))
   (def items (collection src))

   (get items {:id 3})           ; fetch by query
   (seq items)                   ; list all
   (mutate! items nil data)      ; create
   (mutate! items {:id 3} data)  ; update
   (mutate! items {:id 3} nil)   ; delete
   ```

   ## Transactional Mutations

   For queries containing mutations, apply all mutations atomically
   via `transact!`, then query the new state via `snapshot`:

   ```clojure
   (def src (atom-source))

   ;; Apply mutations atomically
   (transact! src
     [{:op :create :data {:title \"Post 1\"}}
      {:op :create :data {:title \"Post 2\"}}])

   ;; Query the new state
   (count (snapshot src))  ;=> 2
   ```

   For multiple collections, call `transact!` on each, then `snapshot`:

   ```clojure
   (def sources {:posts (atom-source) :users (atom-source)})

   (transact! (:users sources) [{:op :create :data {:name \"Alice\"}}])
   (transact! (:posts sources) [{:op :create :data {:title \"Hello\"}}])

   {:users (vals (snapshot (:users sources)))
    :posts (vals (snapshot (:posts sources)))}
   ```")

;;=============================================================================
;; Protocols
;;=============================================================================

(defprotocol DataSource
  "Protocol for CRUD data source backends.

   Implement this for your storage layer (database, atom, API, etc.)."
  (fetch [this query]
    "Fetch item(s) matching query map. Returns item or nil.")
  (list-all [this]
    "List all items. Returns sequence.")
  (create! [this data]
    "Create new item. Returns created item with any generated fields.")
  (update! [this query data]
    "Update item matching query. Returns updated item or nil.")
  (delete! [this query]
    "Delete item matching query. Returns true if deleted, false otherwise."))

(defprotocol Mutable
  "Protocol for collections that support CRUD mutations."
  (mutate! [coll query value]
    "Perform mutation based on query and value:
     - (mutate! coll nil data) -> CREATE
     - (mutate! coll query data) -> UPDATE
     - (mutate! coll query nil) -> DELETE"))

(defprotocol Wireable
  "Protocol for types needing custom wire serialization.

   Implement this for custom types that should be converted to
   standard Clojure data for Transit/EDN serialization."
  (->wire [this]
    "Convert to serializable Clojure data (maps, vectors, etc.)"))

(defprotocol TxSource
  "Protocol for transactional data sources.

   Extends DataSource concept with ability to apply multiple mutations
   atomically and snapshot state for querying."
  (snapshot [this]
    "Get immutable snapshot of current state for querying.")
  (transact! [this mutations]
    "Apply mutations atomically. Returns snapshot after mutations.

     Each mutation is a map:
     {:op :create | :update | :delete
      :query map (for update/delete)
      :data map (for create/update)}

     All mutations succeed or none do."))

;;=============================================================================
;; Collection Type
;;=============================================================================

(deftype Collection [data-source indexes]
  #?@(:clj
      [clojure.lang.ILookup
       (valAt [this query]
              (.valAt this query nil))
       (valAt [_ query not-found]
              (cond
                ;; Primary key lookup (always allowed)
                (and (map? query) (contains? query :id))
                (or (fetch data-source query) not-found)

                ;; Other indexed queries - validate index exists
                (map? query)
                (let [query-keys (set (keys query))]
                  (if (some #(= query-keys %) indexes)
                    (or (fetch data-source query) not-found)
                    (throw (ex-info "No index for query"
                                    {:query query
                                     :available-indexes indexes}))))

                :else not-found))

       clojure.lang.Seqable
       (seq [_]
            (seq (list-all data-source)))

       clojure.lang.Counted
       (count [_]
              (count (list-all data-source)))]

      :cljs
      [ILookup
       (-lookup [this query]
                (-lookup this query nil))
       (-lookup [_ query not-found]
                (cond
                  (and (map? query) (contains? query :id))
                  (or (fetch data-source query) not-found)

                  (map? query)
                  (let [query-keys (set (keys query))]
                    (if (some #(= query-keys %) indexes)
                      (or (fetch data-source query) not-found)
                      (throw (ex-info "No index for query"
                                      {:query query
                                       :available-indexes indexes}))))

                  :else not-found))

       ISeqable
       (-seq [_]
             (seq (list-all data-source)))

       ICounted
       (-count [_]
               (count (list-all data-source)))]))

(extend-type Collection
  Mutable
  (mutate! [coll query value]
    (let [ds (.-data-source coll)]
      (cond
        ;; CREATE: nil query + data
        (and (nil? query) (some? value))
        (create! ds value)

        ;; DELETE: query + nil value
        (and (some? query) (nil? value))
        (delete! ds query)

        ;; UPDATE: query + data
        (and (some? query) (some? value))
        (update! ds query value)

        :else
        (throw (ex-info "Invalid mutation" {:query query :value value})))))

  Wireable
  (->wire [coll]
    (vec (seq coll))))

;;=============================================================================
;; Constructor
;;=============================================================================

(defn collection
  "Create a Collection wrapping a DataSource.

   Options:
   - :indexes - Set of indexed field sets for queries.
                Default: #{#{:id}} (primary key only)
                Example: #{#{:id} #{:author} #{:status :type}}

   The collection implements:
   - ILookup: (get coll {:id 3}) -> fetch by query
   - Seqable: (seq coll) -> list all
   - Counted: (count coll) -> count all
   - Mutable: (mutate! coll query value) -> create/update/delete
   - Wireable: automatically serializes to vector for wire format"
  ([data-source]
   (collection data-source {}))
  ([data-source {:keys [indexes] :or {indexes #{#{:id}}}}]
   (->Collection data-source indexes)))

;;=============================================================================
;; Atom-backed Transactional Source
;;=============================================================================

(defn- apply-mutation
  "Apply a single mutation to db state. Pure function for use in swap!."
  [db id-key id-counter {:keys [op query data]}]
  (case op
    :create
    (let [id (swap! id-counter inc)
          item (assoc data id-key id)]
      (assoc db id item))

    :update
    (if-let [id (get query id-key)]
      (if-let [item (get db id)]
        (assoc db id (merge item data))
        db)
      ;; Query by other fields
      (if-let [item (first (filter #(every? (fn [[k v]] (= (get % k) v)) query)
                                   (vals db)))]
        (assoc db (get item id-key) (merge item data))
        db))

    :delete
    (if-let [id (get query id-key)]
      (dissoc db id)
      ;; Query by other fields
      (if-let [item (first (filter #(every? (fn [[k v]] (= (get % k) v)) query)
                                   (vals db)))]
        (dissoc db (get item id-key))
        db))

    ;; Unknown op - return unchanged
    db))

(defrecord AtomSource [db-atom id-key id-counter]
  DataSource
  (fetch [_ query]
    (let [db @db-atom]
      (if (contains? query id-key)
        (get db (get query id-key))
        (first (filter #(every? (fn [[k v]] (= (get % k) v)) query)
                       (vals db))))))

  (list-all [_]
    (sort-by id-key (vals @db-atom)))

  (create! [_ data]
    (let [id (swap! id-counter inc)
          item (assoc data id-key id)]
      (swap! db-atom assoc id item)
      item))

  (update! [this query data]
    (when-let [item (fetch this query)]
      (let [updated (merge item data)]
        (swap! db-atom assoc (get item id-key) updated)
        updated)))

  (delete! [this query]
    (if-let [item (fetch this query)]
      (do (swap! db-atom dissoc (get item id-key)) true)
      false))

  TxSource
  (snapshot [_]
    @db-atom)

  (transact! [_ mutations]
    (swap! db-atom
           (fn [db]
             (reduce #(apply-mutation %1 id-key id-counter %2)
                     db
                     mutations)))))

(defn atom-source
  "Create an atom-backed transactional data source.

   Implements both DataSource (for individual CRUD) and TxSource
   (for atomic batch mutations).

   Options:
   - :id-key - Primary key field (default :id)
   - :initial - Initial data as map {id -> item} or vector [item ...]

   Examples:
   ```clojure
   (def src (atom-source))
   (def src (atom-source {:id-key :user-id}))
   (def src (atom-source {:initial {1 {:id 1 :name \"Alice\"}}}))
   ```"
  ([] (atom-source {}))
  ([{:keys [id-key initial] :or {id-key :id}}]
   (let [init-map (cond
                    (nil? initial) {}
                    (map? initial) initial
                    (sequential? initial) (into {} (map (juxt id-key identity) initial))
                    :else {})
         max-id (if (seq init-map)
                  (apply max (keys init-map))
                  0)]
     (->AtomSource (atom init-map) id-key (atom max-id)))))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;;---------------------------------------------------------------------------
  ;; Collection with atom-source
  ;;---------------------------------------------------------------------------

  (def src (atom-source))
  (def coll (collection src {:indexes #{#{:id} #{:name}}}))

  ;; CREATE via mutate!
  (:id (mutate! coll nil {:name "Alice"})) ;=> 1
  (:id (mutate! coll nil {:name "Bob"})) ;=> 2

  ;; READ via ILookup
  (:name (get coll {:id 1})) ;=> "Alice"
  (:name (get coll {:name "Bob"})) ;=> "Bob"

  ;; LIST via seq
  (count (seq coll)) ;=> 2

  ;; UPDATE
  (:name (mutate! coll {:id 1} {:name "Alice Updated"})) ;=> "Alice Updated"

  ;; DELETE
  (mutate! coll {:id 2} nil) ;=> true
  (count coll) ;=> 1

  ;; Query without index throws
  (ex-data
   (try (get coll {:age 30})
        (catch Exception e e))) ;=>> {:query {:age 30}}

  ;; Wireable - converts to vector
  (->wire coll) ;=>> vector?

  ;;---------------------------------------------------------------------------
  ;; Transactional Operations via transact! + snapshot
  ;;---------------------------------------------------------------------------

  (def tx-src (atom-source))

  ;; Multiple creates in one transaction
  (transact! tx-src
             [{:op :create :data {:title "A"}}
              {:op :create :data {:title "B"}}
              {:op :create :data {:title "C"}}])
  (count (snapshot tx-src)) ;=> 3

  ;; Mixed operations in one transaction
  (transact! tx-src
             [{:op :create :data {:title "D"}}
              {:op :update :query {:id 1} :data {:title "A-updated"}}
              {:op :delete :query {:id 2}}])
  (count (snapshot tx-src)) ;=> 3  ;; added 1, deleted 1

  ;; Verify mutations applied correctly
  (:title (get (snapshot tx-src) 1)) ;=> "A-updated"
  (get (snapshot tx-src) 2) ;=> nil

  ;;---------------------------------------------------------------------------
  ;; Multiple Collections
  ;;---------------------------------------------------------------------------

  (def sources {:posts (atom-source) :users (atom-source)})

  ;; Transact on each source
  (transact! (:users sources)
             [{:op :create :data {:name "Alice"}}
              {:op :create :data {:name "Bob"}}])
  (transact! (:posts sources)
             [{:op :create :data {:title "Hello" :author-id 1}}])

  ;; Query snapshots
  (count (snapshot (:users sources))) ;=> 2
  (count (snapshot (:posts sources))) ;=> 1
  (set (map :name (vals (snapshot (:users sources))))) ;=> #{"Alice" "Bob"}
  )
