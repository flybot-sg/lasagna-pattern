# Collection

The component that makes patterns bidirectional — enabling read AND write through the same pull syntax.

## Why This Component Exists

Without `collection`, patterns are read-only (SELECT). `collection` introduces the `Mutable` protocol, which elevates patterns to full read-write round-trips: a mutation pattern like `{:posts {nil data}}` creates an entity AND returns it in the response.

This is what makes the pull-pattern system fundamentally different from REST or GraphQL: **every interaction is a round-trip that returns data**, whether it's a read or a write.

`collection` wraps any data source (Datahike, atoms, APIs, files) in a uniform `ILookup + Seqable + Mutable` interface. `pattern` consumes `ILookup` for reads. `remote` calls `mutate!` for writes and sends the result back to the client. The result must be used — discarding mutation responses and re-fetching is an anti-pattern.

### Mutation Return Values

`mutate!` delegates to `DataSource` methods that return meaningful data:

| Operation | DataSource method | Must return |
|-----------|-------------------|-------------|
| CREATE (`nil` query, some value) | `create!` | Full created entity (with generated ID, timestamps) |
| UPDATE (some query, some value) | `update!` | Full updated entity |
| DELETE (some query, `nil` value) | `delete!` | `true` / `false` |

These return values flow through `remote` back to the client. `DataSource` implementations must return complete entities — not partial data.

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
    ├──► read-only       (disables Mutable)
    ├──► wrap-mutable    (custom mutation logic, delegates reads)
    └──► pattern matching / remote serving (consumes ILookup + Mutable)

Non-enumerable resources (no DataSource needed):
    │
    ▼
lookup ← ILookup/Wireable from keyword→value map (delays auto-derefed)
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
(def public-posts (coll/read-only posts))              ;; guest access
(def member-posts (coll/wrap-mutable posts mutate-fn)) ;; ownership enforcement
```

### 2. Custom wrappers for authorization logic

Don't put authorization in DataSource. Use `wrap-mutable` to add access control
while delegating reads to the inner collection:

```clojure
;; CORRECT: wrap-mutable adds ownership check, DataSource stays generic
(defn member-posts [posts user-id]
  (coll/wrap-mutable posts
    (fn [posts query value]
      (cond
        ;; CREATE: inject author
        (and (nil? query) (some? value))
        (coll/mutate! posts nil (assoc value :post/author user-id))

        ;; UPDATE/DELETE: check ownership
        (some? query)
        (if (owns-post? posts user-id query)
          (coll/mutate! posts query value)
          {:error {:type :forbidden :message "You don't own this post"}})))))
```

This keeps DataSource reusable across different access contexts.

### 3. Field lookup for non-enumerable resources

Some resources (profile, user-info) can't be listed — only looked up by keyword.
Use `coll/lookup` with delays for expensive fields:

```clojure
;; CORRECT: lookup with delay-based laziness
(defn me-lookup [conn session]
  (coll/lookup {:id      (:user-id session)           ;; cheap — used as-is
                :email   (:user-email session)         ;; cheap
                :slug    (delay (db-lookup conn uid))  ;; expensive — computed once
                :roles   (or (:roles session) #{})}))  ;; cheap
```

Delays are shared between ILookup and `->wire` — the DB query runs at most once
regardless of whether the value is accessed via pattern matching (ILookup) or
serialization (Wireable).

For resources keyed by query maps (not keywords), use `reify` with ILookup + Wireable:

```clojure
;; CORRECT: query-keyed lookup (can't use coll/lookup — needs map keys)
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

### Mistake: Field filtering in DataSource instead of letting patterns select

```clojure
;; WRONG: list-all curates fields — the DataSource is doing the pattern's job
(list-all [_]
  (d/q '[:find [(pull ?e [:post/id :post/title {:post/author [:user/name]}]) ...]
         :where [?e :post/id _]]
       @conn))

;; RIGHT: list-all returns complete entities, same as fetch
(list-all [_]
  (d/q '[:find [(pull ?e [* {:post/author [*]}]) ...]
         :where [?e :post/id _]]
       @conn))
```

`DataSource` returns data. The pattern system selects shape. When `list-all` pre-filters fields (e.g., omitting `:post/content` for "lightweight" lists), it breaks the pull-pattern contract — the client can't request fields the DataSource already stripped. The same pull expression (`[*]` or equivalent) should be used in both `fetch` and `list-all`.

Format normalization (stripping `:db/id`, converting sets to vectors) is fine — that's adapting storage representation to domain representation. But omitting domain fields is not.

Also ensure `fetch` and `list-all` return the **same shape**. If `list-all` includes roles but `fetch` doesn't (or vice versa), consumers get inconsistent data depending on whether they access a single item or list all.

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
| `Wireable` | `->wire` | Serialize for Transit/EDN (collections -> vectors, lookups -> maps or nil) |
| `TxSource` | `snapshot`, `transact!` | Atomic batch mutations (atom-source only) |

## Constructors & Wrappers

| Function | Purpose | Implements |
|---|---|---|
| `collection` | Wraps DataSource for pattern-compatible CRUD | ILookup, Seqable, Counted, Mutable, Wireable |
| `read-only` | Disables mutations on a collection | ILookup, Seqable, Counted, Wireable |
| `wrap-mutable` | Custom mutation logic, delegates reads | ILookup, Seqable, Counted, Mutable, Wireable |
| `lookup` | Non-enumerable keyword→value resource | ILookup, Wireable |
| `atom-source` | In-memory DataSource + TxSource | DataSource, TxSource |

## Testing

```bash
bb test collection
```

All tests are inline RCT. When adding new DataSource implementations or wrappers, add RCT tests in the same file covering CRUD, edge cases (empty query, missing index), and Wireable output.
