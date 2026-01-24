(ns sg.flybot.pullable.collection
  "CRUD collection abstraction.

   Provides a generic Collection type that wraps a DataSource backend
   and implements ILookup, Seqable, Counted for pattern-based access.

   ## Usage

   1. Implement DataSource for your backend:

   ```clojure
   (defrecord MyDB [db-atom]
     DataSource
     (fetch [_ query] (get @db-atom (:id query)))
     (list-all [_] (vals @db-atom))
     (create! [_ data] (swap! db-atom assoc (:id data) data) data)
     (update! [_ query data] ...)
     (delete! [_ query] ...))
   ```

   2. Create a collection:

   ```clojure
   (def items (collection (->MyDB db) {:indexes #{#{:id}}}))
   ```

   3. Use via ILookup and Seqable:

   ```clojure
   (get items {:id 3})       ; fetch by query
   (seq items)               ; list all
   (mutate! items nil data)  ; create
   (mutate! items query data) ; update
   (mutate! items query nil)  ; delete
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
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; Test DataSource with atom backend
  (defrecord AtomDS [db-atom id-counter]
    DataSource
    (fetch [_ query]
      (if (contains? query :id)
        (get @db-atom (:id query))
        (first (filter #(every? (fn [[k v]] (= (get % k) v)) query)
                       (vals @db-atom)))))
    (list-all [_]
      (sort-by :id (vals @db-atom)))
    (create! [_ data]
      (let [id (swap! id-counter inc)
            item (assoc data :id id)]
        (swap! db-atom assoc id item)
        item))
    (update! [_ query data]
      (when-let [item (fetch _ query)]
        (let [updated (merge item data)]
          (swap! db-atom assoc (:id item) updated)
          updated)))
    (delete! [_ query]
      (if-let [item (fetch _ query)]
        (do (swap! db-atom dissoc (:id item)) true)
        false)))

  ;; Setup
  (def db (atom {}))
  (def counter (atom 0))
  (def ds (->AtomDS db counter))
  (def coll (collection ds {:indexes #{#{:id} #{:name}}}))

  ;; CREATE
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

  ;; Cleanup
  (reset! db {})
  (reset! counter 0))
