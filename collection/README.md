# collection

[![lasagna-collection](https://img.shields.io/clojars/v/sg.flybot/lasagna-collection.svg)](https://clojars.org/sg.flybot/lasagna-collection) ![CI](https://github.com/flybot-sg/lasagna-pattern/actions/workflows/ci.yml/badge.svg) ![License: Unlicense](https://img.shields.io/badge/license-Unlicense-blue.svg)

Part of the [Lasagna Pattern](https://github.com/flybot-sg/lasagna-pattern) toolbox.

CRUD collection abstraction that wraps any data source in a uniform `ILookup + Seqable + Mutable` interface.

## Rationale

Different data stores (databases, APIs, in-memory maps) have different interfaces, making it hard to write reusable data access code. This library provides a **uniform abstraction** so that:

- **`get` and `seq` just work** — Your collection implements `ILookup` and `Seqable`, so it behaves like a Clojure map to any consumer
- **One DataSource, many access levels** — Implement your storage logic once, then compose behavior with wrappers (`read-only`, `wrap-mutable`) instead of duplicating code per role
- **Pattern compatible** — Works seamlessly with the pattern DSL's indexed lookup and the remote HTTP transport

The key design insight is the **decorator pattern**: instead of writing separate DataSource implementations per access level, you write one and stack thin wrappers on top:

```clojure
(def posts (coll/collection (->MyDataSource conn) {:id-key :post/id}))

;; Same DataSource, different access levels
(def public     (coll/read-only posts))                     ; no writes
(def restricted (coll/wrap-mutable posts ownership-check))  ; custom mutation logic
posts                                                       ; unrestricted
```

## Installation

```clojure
;; deps.edn
{:deps {sg.flybot/lasagna-collection {:mvn/version "RELEASE"}}}

;; Leiningen
[sg.flybot/lasagna-collection "RELEASE"]
```

## Quick Start

### With atom-source (in-memory, for testing)

```clojure
(require '[sg.flybot.pullable.collection :as coll])

(def src (coll/atom-source))
(def items (coll/collection src))

;; Standard Clojure verbs
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

Implement the `DataSource` protocol for your storage layer — that's all it takes:

```clojure
(defrecord MyDataSource [conn]
  coll/DataSource
  (fetch [_ query] ...)         ; query is always a map, e.g. {:post/id 3}
  (list-all [_] ...)            ; return seq of all items
  (create! [_ data] ...)        ; return the created item (with generated ID)
  (update! [_ query data] ...)  ; return updated item or nil
  (delete! [_ query] ...))      ; return true/false

(def posts (coll/collection (->MyDataSource conn)
                            {:id-key  :post/id
                             :indexes #{#{:post/id}}}))
```

Then `get`, `seq`, and `mutate!` work immediately. See [examples/flybot-site/.../db/post.cljc](../examples/flybot-site/src/sg/flybot/flybot_site/server/system/db/post.cljc) for a complete Datahike implementation.

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

Wire serialization protocol for Transit/EDN encoding. Converts custom types to plain Clojure data for HTTP transport.

```clojure
(defprotocol Wireable
  (->wire [this] "Convert to serializable Clojure data."))
```

Collections serialize to vectors. Custom types implement their own conversion:

```clojure
;; Non-enumerable lookup — can't list all, serialize as nil
(reify coll/Wireable
  (->wire [_] nil))
```

### TxSource

Transactional data source protocol for atomic batch mutations (implemented by `atom-source`).

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

Disables mutations while keeping all read operations:

```clojure
(def public (coll/read-only posts))

(seq public)                    ; works
(get public {:post/id 1})       ; works
(coll/mutate! public ...)       ; throws — Mutable not implemented
```

### wrap-mutable

Custom mutation logic (e.g., authorization, field injection) while delegating reads to the inner collection:

```clojure
(def restricted
  (coll/wrap-mutable posts
    (fn [inner query value]
      (cond
        ;; CREATE: inject author
        (and (nil? query) (some? value))
        (coll/mutate! inner nil (assoc value :author current-user))

        ;; UPDATE/DELETE: check ownership
        (some? query)
        (if (owns? current-user query)
          (coll/mutate! inner query value)
          {:error {:type :forbidden}})))))

(seq restricted)                                ; delegates to posts
(get restricted {:post/id 1})                   ; delegates to posts
(coll/mutate! restricted nil {:title "New"})    ; runs custom fn
```

### lookup

Non-enumerable keyword-keyed resources where some fields are cheap and others require expensive computation:

```clojure
(def info (coll/lookup {:id     42
                        :name   "Alice"
                        :stats  (delay (expensive-db-query conn 42))}))  ; lazy

(:id info)          ;=> 42 (cheap, no delay)
(:stats info)       ;=> runs expensive-db-query once (delay), caches result
(coll/->wire info)  ;=> {:id 42 :name "Alice" :stats {...}}
```

Delay values are dereferenced transparently on access. Shared between `ILookup` and `->wire` — a DB query runs at most once.

**Note:** `lookup` only supports keyword keys. For map-keyed queries (e.g., `{:post/id 3}`), use `reify` with `ILookup` + `Wireable` directly.

## Collection Options

```clojure
(coll/collection data-source
  {:id-key  :post/id                ; primary key field (default :id)
   :indexes #{#{:post/id}           ; indexed field sets for queries
              #{:post/author}}})    ; allows (get coll {:post/author "alice"})
```

Queries must match a declared index or include the `id-key`, otherwise throws `"No index for query"`.

## Public API

| Function | Signature | Description |
|----------|-----------|-------------|
| `collection` | `[src]` or `[src opts]` | Create Collection wrapping a DataSource |
| `atom-source` | `[]` or `[opts]` | Create atom-backed DataSource + TxSource |
| `read-only` | `[coll]` | Wrap collection to disable mutations |
| `wrap-mutable` | `[coll mutate-fn]` | Wrap collection with custom mutation logic |
| `lookup` | `[field-map]` | Create ILookup + Wireable from keyword→value map |
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
