# Collection

CRUD collection abstraction that keeps `pattern` and `remote` generic by providing a dedicated place for client-specific data logic.

## Why This Component Exists

`pattern` matches data. `remote` serves it over HTTP. Neither knows how your data is stored or what rules govern access. **Collection bridges that gap**: it wraps any data source (Datahike, atoms, APIs, files) in a uniform `ILookup + Seqable + Mutable` interface that pattern matching and remote serving consume without coupling to your storage layer.

Without collection, every consumer of pattern/remote would reinvent CRUD wrappers, query validation, serialization, and access control inline. Collection provides the canonical place for all of that.

## Core Design: Protocol + Wrapper + Composition

```
Your storage (Datahike, atom, API, file...)
    │
    ▼
DataSource protocol ← you implement this (5 methods)
    │
    ▼
Collection type ← uniform ILookup/Seqable/Mutable/Wireable
    │
    ├──► read-only wrapper (disables Mutable)
    ├──► custom wrapper (adds authorization, ownership, etc.)
    └──► pattern matching / remote serving (consumes ILookup + Mutable)
```

## How to Support a New Data Source

Implement the `DataSource` protocol — that's all it takes:

```clojure
(defrecord MySource [conn]
  coll/DataSource
  (fetch [_ query] ...)       ;; query is always a map, e.g. {:post/id 3}
  (list-all [_] ...)          ;; return seq of all items
  (create! [_ data] ...)      ;; return the created item (with generated ID)
  (update! [_ query data] ...)  ;; return updated item or nil
  (delete! [_ query] ...))    ;; return true/false

(def my-coll (coll/collection (->MySource conn)
                              {:id-key :post/id
                               :indexes #{#{:post/id}}}))
```

Then pattern matching works immediately:

```clojure
(seq my-coll)                    ;; list all
(get my-coll {:post/id 3})      ;; fetch by indexed query
(coll/mutate! my-coll nil data) ;; create
```

### Built-in: atom-source

For in-memory / testing / browser-side use:

```clojure
(def src (coll/atom-source {:initial [{:id 1 :name "Alice"}]
                            :id-key :id}))
(def items (coll/collection src))
```

`atom-source` also implements `TxSource` for atomic batch mutations via `transact!`/`snapshot`.

## Critical: How Collection Interacts with Pattern and Remote

### Pattern sees ILookup

`pattern` matches against any `ILookup`. When it encounters a map key like `{:post/id 3}`, it calls `(get coll {:post/id 3})`. Collection validates the query against its indexes, then delegates to `DataSource/fetch`. Pattern never calls `mutate!` — it only reads.

### Remote detects mutations and calls mutate!

`remote/http.cljc` parses the incoming pattern to detect mutations:

| Pattern shape | Operation | What remote does |
|---|---|---|
| `{:posts '?all}` | Read | Compiles pattern, matches against data |
| `{:posts {nil {:title "X"}}}` | Create | Calls `(coll/mutate! posts nil {:title "X"})` |
| `{:posts {{:id 3} {:title "Y"}}}` | Update | Calls `(coll/mutate! posts {:id 3} {:title "Y"})` |
| `{:posts {{:id 3} nil}}` | Delete | Calls `(coll/mutate! posts {:id 3} nil)` |

Remote checks `(satisfies? coll/Mutable coll)` before mutating. If false (e.g. `read-only` wrapper), it returns an error.

### Wireable for serialization

Remote walks the result tree calling `(coll/->wire x)` on any `Wireable`. Collections serialize to vectors. Custom lookups can serialize to nil (lazy, non-enumerable) or to custom maps.

## Proper Usage Patterns

### 1. One DataSource per storage backend, one Collection per access path

```clojure
;; CORRECT: single DataSource, multiple Collection wrappers
(def posts-ds (->PostsDataSource conn))
(def posts (coll/collection posts-ds {:id-key :post/id}))
(def public-posts (coll/read-only posts))    ;; guest access
(def member-posts (->MemberPosts posts uid)) ;; ownership enforcement
```

### 2. Custom wrappers for authorization logic

Don't put authorization in DataSource. Put it in a wrapper type:

```clojure
;; CORRECT: wrapper adds access control, DataSource stays generic
(deftype MemberPosts [posts user-id]
  ILookup  ;; delegate reads to underlying posts
  Mutable  ;; check ownership before delegating mutations
  Wireable ;; delegate serialization)
```

This keeps DataSource reusable across different access contexts.

### 3. Lazy ILookup for non-enumerable resources

Some resources (history, profile, user-info) can't be listed — only looked up by key. Use `reify` with ILookup + Wireable:

```clojure
;; CORRECT: lazy lookup that only fetches on access
(defn history-lookup [conn]
  (reify
    clojure.lang.ILookup
    (valAt [_ query]
      (when-let [id (:post/id query)]
        (post-history @conn id)))
    (valAt [this q nf] (or (.valAt this q) nf))

    coll/Wireable
    (->wire [_] nil)))  ;; can't enumerate all history, serialize as nil
```

### 4. Index configuration must match your query patterns

```clojure
;; If pattern will do: (get coll {:post/author "alice"})
;; Then collection MUST have that index:
(coll/collection src {:id-key :post/id
                      :indexes #{#{:post/id} #{:post/author}}})

;; Without the index, get throws "No index for query"
```

## Common Mistakes

### Mistake: Recreating collections on every request

```clojure
;; WRONG: creates new Collection objects every time the API is called
(defn api-fn [request]
  {:data {:posts (coll/collection (->PostsDataSource conn))}})

;; Collection + DataSource constructors are cheap, but this prevents
;; any caching/memoization and confuses the object identity that
;; wrappers rely on.
```

```clojure
;; RIGHT: create collections once, close over them
(defn make-api [{:keys [conn]}]
  (let [posts (coll/collection (->PostsDataSource conn) {:id-key :post/id})]
    (fn [request]
      {:data {:posts (coll/read-only posts)}})))
```

**Exception**: wrappers that depend on per-request context (like `MemberPosts` which needs the user-id from the session) must be created per-request. But the underlying collection/DataSource should be stable.

### Mistake: Confusing DataSource and Collection

```clojure
;; WRONG: passing atom-source directly where Collection is expected
(def src (coll/atom-source))
(get src {:id 1})  ;; src is a DataSource, not ILookup!

;; RIGHT: wrap in collection first
(def items (coll/collection src))
(get items {:id 1})  ;; Collection implements ILookup
```

`DataSource` has `fetch/list-all/create!/update!/delete!`.
`Collection` has `ILookup/Seqable/Counted/Mutable/Wireable`.
Pattern matching and remote only work with Collection (or any ILookup).

### Mistake: Mutating a read-only collection

```clojure
;; WRONG: trying to mutate through read-only wrapper
(def public (coll/read-only posts))
(coll/mutate! public nil {:title "X"})  ;; throws — Mutable not satisfied

;; RIGHT: use the mutable collection for writes
(coll/mutate! posts nil {:title "X"})
```

`read-only` deliberately does NOT implement `Mutable`. Remote checks `(satisfies? coll/Mutable coll)` and returns an error if false.

### Mistake: Returning errors as exceptions from DataSource

```clojure
;; WRONG in custom wrappers: throwing on business logic errors
(mutate! [_ query value]
  (if (owns? query)
    (coll/mutate! inner query value)
    (throw (ex-info "Forbidden" {}))))   ;; remote catches this as :execution-error (500)

;; RIGHT: return error as data — remote's :detect picks it up
(mutate! [_ query value]
  (if (owns? query)
    (coll/mutate! inner query value)
    {:error {:type :forbidden :message "You don't own this post"}}))
```

Remote's error config uses `:detect :error` to check if the mutation result contains an `:error` key, then maps `:type` to HTTP status codes.

## Protocols Reference

| Protocol | Methods | Purpose |
|---|---|---|
| `DataSource` | `fetch`, `list-all`, `create!`, `update!`, `delete!` | Backend storage adapter |
| `Mutable` | `mutate!` | Unified CRUD: `(nil, data)` = create, `(query, data)` = update, `(query, nil)` = delete |
| `Wireable` | `->wire` | Serialize for Transit/EDN (collections -> vectors, lazy lookups -> nil) |
| `TxSource` | `snapshot`, `transact!` | Atomic batch mutations (atom-source only) |

## Testing

```bash
bb test collection
```

All tests are inline RCT. When adding new DataSource implementations or wrappers, add RCT tests in the same file covering CRUD, edge cases (empty query, missing index), and Wireable output.
