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

(defn- lookup-by-query
  "Core lookup logic shared by CLJ and CLJS implementations."
  [data-source id-key indexes query not-found]
  (cond
    (not (map? query))
    not-found

    (empty? query)
    (throw (ex-info "Empty query not allowed" {:query query}))

    (contains? query id-key)
    (or (fetch data-source query) not-found)

    (some #(= (set (keys query)) %) indexes)
    (or (fetch data-source query) not-found)

    :else
    (throw (ex-info "No index for query"
                    {:query query :available-indexes indexes}))))

(deftype Collection [data-source id-key indexes]
  #?@(:clj
      [clojure.lang.ILookup
       (valAt [this query]
              (.valAt this query nil))
       (valAt [_ query not-found]
              (lookup-by-query data-source id-key indexes query not-found))

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
                (lookup-by-query data-source id-key indexes query not-found))

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
        (and (nil? query) (some? value))  (create! ds value)
        (and (some? query) (nil? value))  (delete! ds query)
        (and (some? query) (some? value)) (update! ds query value)
        :else (throw (ex-info "Invalid mutation" {:query query :value value})))))

  Wireable
  (->wire [coll]
    (vec (seq coll))))

;;=============================================================================
;; Read-Only Wrapper
;;=============================================================================

(deftype ReadOnly [coll]
  #?@(:clj
      [clojure.lang.ILookup
       (valAt [_ query]
              (get coll query))
       (valAt [_ query not-found]
              (get coll query not-found))

       clojure.lang.Seqable
       (seq [_]
            (seq coll))

       clojure.lang.Counted
       (count [_]
              (count coll))]

      :cljs
      [ILookup
       (-lookup [_ query]
                (get coll query))
       (-lookup [_ query not-found]
                (get coll query not-found))

       ISeqable
       (-seq [_]
             (seq coll))

       ICounted
       (-count [_]
               (count coll))]))

(extend-type ReadOnly
  Wireable
  (->wire [this]
    (->wire (.-coll this))))

(defn read-only
  "Wrap a collection to make it read-only.

   The returned collection supports ILookup, Seqable, Counted, and Wireable,
   but NOT Mutable. Attempts to mutate via pattern will fail with
   'collection not mutable' error.

   Use this for public API endpoints where reads are allowed but writes
   should be gated behind authentication.

   Example:
   ```clojure
   (def posts (collection (atom-source)))
   (def public-posts (read-only posts))

   (seq public-posts)           ; works
   (get public-posts {:id 1})   ; works
   (mutate! public-posts ...)   ; throws - Mutable not implemented
   ```"
  [coll]
  (->ReadOnly coll))

;;=============================================================================
;; Field Lookup (non-enumerable keyword-keyed resources)
;;=============================================================================

(defn- deref-if-delay
  "Deref delays, pass other values through."
  [v]
  (if (delay? v) @v v))

(deftype FieldLookup [field-map]
  #?@(:clj
      [clojure.lang.ILookup
       (valAt [this k]
              (.valAt this k nil))
       (valAt [_ k not-found]
              (let [v (get field-map k ::not-found)]
                (if (= v ::not-found) not-found (deref-if-delay v))))]

      :cljs
      [ILookup
       (-lookup [this k]
                (-lookup this k nil))
       (-lookup [_ k not-found]
                (let [v (get field-map k ::not-found)]
                  (if (= v ::not-found) not-found (deref-if-delay v))))]))

(extend-type FieldLookup
  Wireable
  (->wire [this]
    (persistent!
     (reduce-kv (fn [m k v] (assoc! m k (deref-if-delay v)))
                (transient {})
                (.-field-map this)))))

(defn lookup
  "Create an ILookup + Wireable from a keyword->value map.

   Delay values are dereferenced transparently on access.
   `->wire` produces a plain map with all delays forced.

   Use for non-enumerable resources where fields come from
   multiple sources with different costs:

   ```clojure
   (lookup {:id    user-id                          ; cheap — used as-is
            :email (:email session)                  ; cheap — used as-is
            :slug  (delay (db-lookup conn user-id))  ; expensive — computed once
            :roles (or (:roles session) #{})})       ; cheap — used as-is
   ```

   Delays are shared between ILookup and ->wire — a DB query
   runs at most once regardless of access path."
  [field-map]
  (->FieldLookup field-map))

;;=============================================================================
;; Mutable Wrapper
;;=============================================================================

(deftype MutableWrapper [coll mutate-fn]
  #?@(:clj
      [clojure.lang.ILookup
       (valAt [_ query]
              (get coll query))
       (valAt [_ query not-found]
              (get coll query not-found))

       clojure.lang.Seqable
       (seq [_]
            (seq coll))

       clojure.lang.Counted
       (count [_]
              (count coll))]

      :cljs
      [ILookup
       (-lookup [_ query]
                (get coll query))
       (-lookup [_ query not-found]
                (get coll query not-found))

       ISeqable
       (-seq [_]
             (seq coll))

       ICounted
       (-count [_]
               (count coll))]))

(extend-type MutableWrapper
  Mutable
  (mutate! [this query value]
    ((.-mutate-fn this) (.-coll this) query value))

  Wireable
  (->wire [this]
    (->wire (.-coll this))))

(defn wrap-mutable
  "Wrap a collection with custom mutation logic, delegating reads.

   mutate-fn receives (coll query value) and should return:
   - For create (nil query, some value): the created item
   - For update (some query, some value): the updated item
   - For delete (some query, nil value): true/false
   - For errors: {:error {:type ... :message ...}}

   Reads (ILookup, Seqable, Counted) and Wireable delegate to the
   inner collection. Use this to add authorization, ownership checks,
   or field injection without reimplementing the full deftype boilerplate.

   Example:
   ```clojure
   (def member-posts
     (wrap-mutable posts
       (fn [posts query value]
         (if (owns? user-id query)
           (mutate! posts query value)
           {:error {:type :forbidden}}))))
   ```"
  [coll mutate-fn]
  (->MutableWrapper coll mutate-fn))

;;=============================================================================
;; Constructor
;;=============================================================================

(defn collection
  "Create a Collection wrapping a DataSource.

   Options:
   - :id-key  - Primary key field (default :id). Must match data-source config.
   - :indexes - Set of indexed field sets for queries.
                Default: #{#{<id-key>}} (primary key only)
                Example: #{#{:id} #{:author} #{:status :type}}

   The collection implements:
   - ILookup: (get coll {:id 3}) -> fetch by query
   - Seqable: (seq coll) -> list all
   - Counted: (count coll) -> count all
   - Mutable: (mutate! coll query value) -> create/update/delete
   - Wireable: automatically serializes to vector for wire format"
  ([data-source]
   (collection data-source {}))
  ([data-source {:keys [id-key indexes]}]
   (let [id-key (or id-key :id)
         indexes (or indexes #{#{id-key}})]
     (->Collection data-source id-key indexes))))

;;=============================================================================
;; Atom-backed Transactional Source
;;=============================================================================

(defn- matches-query?
  "Check if item matches all key-value pairs in query."
  [item query]
  (every? (fn [[k v]] (= (get item k) v)) query))

(defn- find-by-query
  "Find first item in db matching query. Looks up by id-key first if present."
  [db id-key query]
  (when (empty? query)
    (throw (ex-info "Empty query not allowed" {:query query})))
  (if-let [id (get query id-key)]
    (get db id)
    (first (filter #(matches-query? % query) (vals db)))))

(defn- apply-mutation
  "Apply a single mutation to db state. Pure function for use in swap!.
   For :create, next-id is the pre-computed ID (computed outside swap!)."
  [db id-key next-id {:keys [op query data]}]
  (case op
    :create (assoc db next-id (assoc data id-key next-id))
    :update (if-let [item (find-by-query db id-key query)]
              (assoc db (get item id-key) (merge item data))
              db)
    :delete (if-let [item (find-by-query db id-key query)]
              (dissoc db (get item id-key))
              db)
    (throw (ex-info "Unknown mutation op" {:op op :valid-ops #{:create :update :delete}}))))

(defrecord AtomSource [db-atom id-key id-counter]
  DataSource
  (fetch [_ query]
    (find-by-query @db-atom id-key query))

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
    ;; Pre-compute IDs for all creates BEFORE swap! to ensure atomicity.
    (let [create-count (count (filter #(= :create (:op %)) mutations))
          start-id (- (swap! id-counter + create-count) create-count)]
      (swap! db-atom
             (fn [db]
               (first
                (reduce (fn [[db next-id] mut]
                          [(apply-mutation db id-key next-id mut)
                           (if (= :create (:op mut)) (inc next-id) next-id)])
                        [db (inc start-id)]
                        mutations)))))))

(defn atom-source
  "Create an atom-backed transactional data source.

   Implements both DataSource (for individual CRUD) and TxSource
   (for atomic batch mutations).

   Options:
   - :id-key - Primary key field (default :id)
   - :initial - Initial data as map {id -> item} or vector [item ...]

   Note: IDs must be positive integers. Auto-generated IDs start from 1
   and increment. If providing initial data, all keys must be integers.

   Examples:
   ```clojure
   (def src (atom-source))
   (def src (atom-source {:id-key :user-id}))
   (def src (atom-source {:initial {1 {:id 1 :name \"Alice\"}}}))
   (def src (atom-source {:initial [{:id 10 :name \"Bob\"}]}))
   ```"
  ([] (atom-source {}))
  ([{:keys [id-key initial] :or {id-key :id}}]
   (let [init-map (cond
                    (nil? initial) {}
                    (map? initial) initial
                    (sequential? initial) (into {} (map (juxt id-key identity) initial))
                    :else {})
         _ (when (seq init-map)
             (when-not (every? integer? (keys init-map))
               (throw (ex-info "Initial data keys must be integers"
                               {:keys (keys init-map)
                                :hint "atom-source uses auto-incrementing integer IDs"}))))
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

  ;; Multiple creates in one transaction - IDs should be sequential
  (transact! tx-src
             [{:op :create :data {:title "A"}}
              {:op :create :data {:title "B"}}
              {:op :create :data {:title "C"}}])
  (count (snapshot tx-src)) ;=> 3
  (sort (keys (snapshot tx-src))) ;=> '(1 2 3)

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

  ;;---------------------------------------------------------------------------
  ;; Empty query validation
  ;;---------------------------------------------------------------------------

  (def empty-src (atom-source))
  (create! empty-src {:name "Test"})

  (ex-message
   (try (fetch empty-src {})
        (catch Exception e e))) ;=> "Empty query not allowed"

  (def empty-coll (collection empty-src))
  (ex-message
   (try (get empty-coll {})
        (catch Exception e e))) ;=> "Empty query not allowed"

  ;;---------------------------------------------------------------------------
  ;; Unknown op throws
  ;;---------------------------------------------------------------------------

  (def unk-src (atom-source))
  (ex-message
   (try (transact! unk-src [{:op :invalid :data {:foo "bar"}}])
        (catch Exception e e))) ;=> "Unknown mutation op"

  ;;---------------------------------------------------------------------------
  ;; Non-integer IDs rejected
  ;;---------------------------------------------------------------------------

  (ex-message
   (try (atom-source {:initial {"a" {:id "a"}}})
        (catch Exception e e))) ;=> "Initial data keys must be integers"

  ;;---------------------------------------------------------------------------
  ;; Custom id-key works correctly
  ;;---------------------------------------------------------------------------

  (def custom-src (atom-source {:id-key :uid}))
  (def custom-coll (collection custom-src {:id-key :uid}))
  (create! custom-src {:name "Alice"})
  (:name (get custom-coll {:uid 1})) ;=> "Alice"

  ;; Query by wrong key throws (not indexed)
  (ex-message
   (try (get custom-coll {:id 1})
        (catch Exception e e))) ;=> "No index for query"

  ;;---------------------------------------------------------------------------
  ;; Read-Only Wrapper
  ;;---------------------------------------------------------------------------

  (def ro-src (atom-source))
  (def ro-coll (collection ro-src {:indexes #{#{:id} #{:name}}}))
  (mutate! ro-coll nil {:name "Alice"})
  (mutate! ro-coll nil {:name "Bob"})

  (def ro (read-only ro-coll))

  ;; Read operations work
  (count ro) ;=> 2
  (count (seq ro)) ;=> 2
  (:name (get ro {:id 1})) ;=> "Alice"
  (:name (get ro {:name "Bob"})) ;=> "Bob"

  ;; Wireable works
  (->wire ro) ;=>> vector?
  (count (->wire ro)) ;=> 2

  ;; Mutable protocol NOT satisfied
  (satisfies? Mutable ro) ;=> false

  ;;---------------------------------------------------------------------------
  ;; Mutable Wrapper
  ;;---------------------------------------------------------------------------

  (def wm-src (atom-source))
  (def wm-coll (collection wm-src {:indexes #{#{:id} #{:name}}}))
  (mutate! wm-coll nil {:name "Alice"})
  (mutate! wm-coll nil {:name "Bob"})

  ;; wrap-mutable with ownership check
  (def owned (wrap-mutable wm-coll
                           (fn [coll query value]
                             (cond
                               (and (nil? query) (some? value))
                               (mutate! coll nil (assoc value :owner "u1"))

                               (and (some? query) (nil? value))
                               {:error {:type :forbidden :message "Cannot delete"}}

                               :else
                               (mutate! coll query value)))))

  ;; Reads delegate to inner collection
  (count owned) ;=> 2
  (:name (get owned {:id 1})) ;=> "Alice"

  ;; Create injects :owner
  (:owner (mutate! owned nil {:name "Charlie"})) ;=> "u1"

  ;; Delete returns error (ownership denied)
  (:type (:error (mutate! owned {:id 1} nil))) ;=> :forbidden

  ;; Update delegates through
  (:name (mutate! owned {:id 1} {:name "Alice Updated"})) ;=> "Alice Updated"

  ;; Wireable delegates
  (->wire owned) ;=>> vector?

  ;; Mutable protocol IS satisfied
  (satisfies? Mutable owned) ;=> true

  ;;---------------------------------------------------------------------------
  ;; Field Lookup (non-enumerable keyword-keyed resources)
  ;;---------------------------------------------------------------------------

  ;; Basic lookup with plain values
  (def basic (lookup {:id "u1" :email "a@b.com" :name "Alice"}))
  (:id basic) ;=> "u1"
  (:email basic) ;=> "a@b.com"
  (:missing basic) ;=> nil
  (get basic :name "fallback") ;=> "Alice"
  (get basic :missing "fallback") ;=> "fallback"

  ;; Delays are dereferenced transparently
  (def call-count (atom 0))
  (def lazy (lookup {:cheap "fast"
                     :expensive (delay (do (swap! call-count inc) "computed"))}))

  ;; Not yet computed
  @call-count ;=> 0

  ;; ILookup access forces the delay
  (:expensive lazy) ;=> "computed"
  @call-count ;=> 1

  ;; Second access reuses cached value (delay fires once)
  (:expensive lazy) ;=> "computed"
  @call-count ;=> 1

  ;; ->wire produces plain map with all delays forced
  (->wire lazy) ;=> {:cheap "fast" :expensive "computed"}
  @call-count ;=> 1

  ;; ->wire on fresh lookup forces delay exactly once
  (def call-count-2 (atom 0))
  (def lazy-2 (lookup {:a 1 :b (delay (do (swap! call-count-2 inc) 2))}))
  (->wire lazy-2) ;=> {:a 1 :b 2}
  @call-count-2 ;=> 1
  ;; ILookup after ->wire still uses same delay
  (:b lazy-2) ;=> 2
  @call-count-2 ;=> 1

  ;; Not Mutable or Seqable — lookup only
  (satisfies? Mutable basic) ;=> false
  )
