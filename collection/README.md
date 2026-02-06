# collection

CRUD collection abstraction with DataSource protocol for lazy data access.

## Rationale

Different data stores (databases, APIs, in-memory maps) have different interfaces, making it hard to write reusable data access code. This library provides a **uniform abstraction** for collections:

- **ILookup + Seqable** - Read with `get` and `seq`, like Clojure maps
- **Mutable protocol** - Single `mutate!` for create/update/delete
- **Composable wrappers** - Add read-only constraints, custom types
- **Pattern compatible** - Works seamlessly with the pattern DSL's indexed lookup

## Installation

```clojure
{:deps {local/collection {:local/root "../collection"}}}
```

## Usage

### Basic (atom-backed storage)

```clojure
(require '[sg.flybot.pullable.collection :as coll])

;; Create a collection with atom-backed storage
(def src (coll/atom-source))
(def items (coll/collection src))

;; READ via ILookup
(seq items)                    ; list all
(get items {:id 1})            ; fetch by query

;; CRUD via mutate!
(coll/mutate! items nil {:name "Alice"})      ; CREATE (nil query)
(coll/mutate! items {:id 1} {:name "Bob"})    ; UPDATE (query + value)
(coll/mutate! items {:id 1} nil)              ; DELETE (query + nil)
```

### With initial data

```clojure
(def users-src
  (coll/atom-source
   {:initial [{:id 1 :name "Alice"}
              {:id 2 :name "Bob"}]}))

(def users (coll/collection users-src))
(count users)  ;=> 2
```

### Custom DataSource (e.g., Datahike)

```clojure
;; Implement DataSource protocol for your storage layer
(defrecord MyDataSource [conn]
  coll/DataSource
  (fetch [_ query] ...)
  (list-all [_] ...)
  (create! [_ data] ...)
  (update! [_ query data] ...)
  (delete! [_ query] ...))

(def posts (coll/collection (->MyDataSource conn)))
```

See `examples/flybot-site/src/.../db.clj` for a complete Datahike implementation.

## Protocols

### DataSource

Backend storage protocol. Implement this for your storage layer.

```clojure
(defprotocol DataSource
  (fetch [this query]    "Fetch item matching query. Returns item or nil.")
  (list-all [this]       "List all items. Returns sequence.")
  (create! [this data]   "Create new item. Returns created item.")
  (update! [this q data] "Update item. Returns updated item or nil.")
  (delete! [this query]  "Delete item. Returns true/false."))
```

### Mutable

Collection mutation protocol. Implemented by `Collection` type.

```clojure
(defprotocol Mutable
  (mutate! [coll query value]
    "CREATE: (mutate! coll nil data)
     UPDATE: (mutate! coll query data)
     DELETE: (mutate! coll query nil)"))
```

### Wireable

Wire serialization protocol for Transit/EDN encoding.

```clojure
(defprotocol Wireable
  (->wire [this] "Convert to serializable Clojure data."))
```

Collections serialize to vectors. Implement on custom types:

```clojure
(deftype MyLookup [...]
  coll/Wireable
  (->wire [_] nil))  ; lazy lookup, not enumerable
```

### TxSource

Transactional data source protocol for atomic batch mutations.

```clojure
(defprotocol TxSource
  (snapshot [this]  "Get immutable snapshot of current state.")
  (transact! [this mutations]
    "Apply mutations atomically. Each mutation:
     {:op :create|:update|:delete, :query map, :data map}"))
```

Example:

```clojure
(def src (coll/atom-source))

(coll/transact! src
  [{:op :create :data {:title "Post 1"}}
   {:op :create :data {:title "Post 2"}}
   {:op :update :query {:id 1} :data {:title "Updated"}}
   {:op :delete :query {:id 2}}])

(count (coll/snapshot src))  ;=> 1
```

## Wrappers

### read-only

Wrap a collection to disable mutations:

```clojure
(def public-posts (coll/read-only posts))

(seq public-posts)              ; works
(get public-posts {:id 1})      ; works
(coll/mutate! public-posts ...) ; throws - Mutable not implemented
```

## Collection Options

```clojure
(coll/collection data-source
  {:id-key  :post/id              ; primary key field (default :id)
   :indexes #{#{:post/id}         ; indexed field sets for queries
              #{:post/author}}})  ; allows (get coll {:post/author "alice"})
```

Queries must match an index or include the id-key, otherwise throws.

## Public API

| Function | Signature | Description |
|----------|-----------|-------------|
| `collection` | `[src]` or `[src opts]` | Create Collection wrapping a DataSource |
| `atom-source` | `[]` or `[opts]` | Create atom-backed DataSource + TxSource |
| `read-only` | `[coll]` | Wrap collection to disable mutations |
| `mutate!` | `[coll query value]` | Protocol: create/update/delete |
| `->wire` | `[x]` | Protocol: convert to serializable data |
| `transact!` | `[src mutations]` | Protocol: atomic batch mutations |
| `snapshot` | `[src]` | Protocol: get immutable state snapshot |

### atom-source options

```clojure
{:id-key  :id       ; primary key field (default :id)
 :initial [...]     ; initial data as vector or {id -> item} map
```

## Development

All commands are run from the **repository root** (see [root README](../README.md) for full task list):

```bash
bb rct collection     # Run RCT tests only
bb test collection    # Run full Kaocha test suite (RCT + integration)
bb dev collection     # Start REPL
```
