# Collection

Makes patterns bidirectional: read AND write through the same pull syntax.

## Role in the Toolbox

Without `collection`, patterns are read-only. `collection` adds `Mutable`, enabling full round-trip CRUD. Every interaction (read or write) returns data. Discarding mutation responses and re-fetching is an anti-pattern.

- `pattern` consumes `ILookup` for reads (calls `get`)
- `remote` calls `mutate!` for writes, sends result back to client
- `collection` wraps any storage in uniform `ILookup + Seqable + Counted + Mutable + Wireable`

## Core Design

```
Your storage (Datahike, atom, API, file...)
    │
    ▼
DataSource protocol ← implement 5 methods (fetch, list-all, create!, update!, delete!)
    │
    ▼
Collection deftype ← uniform ILookup/Seqable/Counted/Mutable/Wireable
    │
    ├──► read-only       (disables Mutable, delegates reads)
    ├──► wrap-mutable    (custom mutation logic, delegates reads)
    └──► consumed by pattern (ILookup) and remote (Mutable)

Non-enumerable resources (no DataSource needed):
    ├──► lookup          keyword→value ILookup/Wireable (delays auto-derefed)
    └──► reify ILookup   map-keyed queries (lookup only supports keyword keys)
```

## How It Works: Clojure Interfaces

Collection uses `deftype` to implement platform interfaces. Each interface overrides a Clojure verb:

| Interface | Verb it powers | What Collection does |
|---|---|---|
| `ILookup` (valAt) | `get` | Validates query against indexes, delegates to `DataSource/fetch` |
| `Seqable` (seq) | `seq`, `map`, `filter` | Delegates to `DataSource/list-all` |
| `Counted` (count) | `count` | Delegates to `DataSource/list-all` + count |
| `Mutable` (mutate!) | `mutate!` (custom) | Routes nil/some query+value to create!/update!/delete! |
| `Wireable` (->wire) | `->wire` (custom) | Serializes to `(vec (seq coll))` for Transit/EDN |

`Mutable` and `Wireable` are project-defined protocols, not Clojure built-ins.

`Wireable` is analogous to `clojure.core.protocols/Datafiable` (`datafy`): both convert opaque types to plain Clojure data. `->wire` is for HTTP transport specifically.

## Mutation Return Values

| Operation | DataSource method | Must return |
|---|---|---|
| CREATE (`nil` query, some value) | `create!` | Full created entity (with generated ID, timestamps) |
| UPDATE (some query, some value) | `update!` | Full updated entity |
| DELETE (some query, `nil` value) | `delete!` | `true` / `false` |

`DataSource` implementations must return complete entities, not partial data.

## Implement a New Data Source

```clojure
(defrecord MySource [conn]
  coll/DataSource
  (fetch [_ query] ...)         ;; query is always a map, e.g. {:post/id 3}
  (list-all [_] ...)            ;; return seq of all items
  (create! [_ data] ...)        ;; return the created item (with generated ID)
  (update! [_ query data] ...)  ;; return updated item or nil
  (delete! [_ query] ...))      ;; return true/false

(def posts (coll/collection (->MySource conn)
                            {:id-key :post/id
                             :indexes #{#{:post/id}}}))
```

Then reads and writes work immediately:

```clojure
(seq posts)                          ;; list all
(get posts {:post/id 3})             ;; fetch by indexed query
(coll/mutate! posts nil {:title "New"}) ;; create
```

### Built-in: atom-source

In-memory DataSource for testing / browser-side use. Also implements `TxSource` for atomic batch mutations via `transact!`/`snapshot`.

```clojure
(def src (coll/atom-source {:initial [{:id 1 :name "Alice"}] :id-key :id}))
(def items (coll/collection src))
```

## Interaction with Pattern and Remote

**READ path**: `remote` compiles pattern via `pattern/match-fn`, matches against data. Pattern calls `(get coll {:post/id 3})`. Collection validates index, delegates to `DataSource/fetch`. Pattern never calls `mutate!`.

**WRITE path**: `remote` detects mutation via `parse-mutation` (variables in value = read, literals = write), walks pattern to find collection, calls `mutate!` directly. `pattern` is not involved.

| Pattern shape | Operation | What remote does |
|---|---|---|
| `{:posts '?all}` | Read | Compiles pattern, matches against data |
| `{:posts {nil {:title "X"}}}` | Create | `(coll/mutate! posts nil {:title "X"})` |
| `{:posts {{:post/id 3} {:title "Y"}}}` | Update | `(coll/mutate! posts {:post/id 3} {:title "Y"})` |
| `{:posts {{:post/id 3} nil}}` | Delete | `(coll/mutate! posts {:post/id 3} nil)` |

Remote checks `(satisfies? coll/Mutable coll)` before mutating. If false (`read-only`), returns error.

**Serialization**: Remote walks result tree calling `(coll/->wire x)` on any `Wireable`. Collections serialize to vectors. Lookups serialize to maps or nil.

## Wrappers and When to Use Each

### One DataSource, one Collection, multiple wrappers

```clojure
(def ds    (->MySource conn))
(def posts (coll/collection ds {:id-key :post/id}))
(def public     (coll/read-only posts))                ;; no writes
(def restricted (coll/wrap-mutable posts auth-fn))     ;; custom mutation logic
;; unrestricted: use posts directly
```

### wrap-mutable for authorization

Keep authorization out of DataSource. Use `wrap-mutable` to intercept mutations:

```clojure
(defn restricted-posts [posts current-user]
  (coll/wrap-mutable posts
    (fn [inner query value]
      (cond
        (and (nil? query) (some? value))
        (coll/mutate! inner nil (assoc value :author current-user))

        (some? query)
        (if (owns? inner current-user query)
          (coll/mutate! inner query value)
          {:error {:type :forbidden :message "Not the author"}})))))
```

### lookup for non-enumerable keyword-keyed resources

**`coll/lookup` only supports keyword keys.** Use for profile-like resources:

```clojure
(coll/lookup {:id     42
              :name   "Alice"
              :stats  (delay (expensive-db-query conn 42))})  ;; computed once
```

Delays shared between ILookup and `->wire` — DB query runs at most once.

### reify for map-keyed read-only lookups

When query keys are maps (not keywords), `coll/lookup` doesn't work. Use `reify`:

```clojure
(defn history-lookup [conn]
  (reify
    clojure.lang.ILookup
    (valAt [_ query]
      (when-let [id (:post/id query)]
        (post-history @conn id)))
    (valAt [this q nf] (or (.valAt this q) nf))

    coll/Wireable
    (->wire [_] nil)))
```

Use when: read-only, non-enumerable, single query shape. The full DataSource/Collection stack would add index validation, Seqable, Mutable — none of which is needed.

### Decision guide: which construct to use

| Need | Tool |
|---|---|
| Full CRUD + enumeration + index validation + wrappers | `defrecord` DataSource + `coll/collection` |
| Read-only, keyword keys, flat values | `coll/lookup` |
| Read-only, map keys, single query shape | Raw `reify` ILookup + Wireable |
| Disable mutations on existing collection | `coll/read-only` |
| Custom mutation logic, delegate reads | `coll/wrap-mutable` |
| Transform read results (not just restrict writes) | `reify` decorator over `read-only` or collection |

## Common Mistakes

### Recreating collections on every request

```clojure
;; WRONG
(defn handler [request]
  {:data {:posts (coll/collection (->MySource conn))}})

;; RIGHT: create once, close over
(defn make-handler [{:keys [conn]}]
  (let [posts (coll/collection (->MySource conn) {:id-key :post/id})]
    (fn [request]
      {:data {:posts (coll/read-only posts)}})))
```

Exception: wrappers depending on per-request context (user-id from session) must be created per-request. The underlying collection/DataSource should be stable.

### Confusing DataSource and Collection

```clojure
;; WRONG
(def src (coll/atom-source))
(get src {:id 1})  ;; DataSource, not ILookup!

;; RIGHT
(def items (coll/collection src))
(get items {:id 1})  ;; Collection implements ILookup
```

### Mutating a read-only collection

```clojure
;; WRONG
(coll/mutate! (coll/read-only posts) nil {:title "X"})  ;; Mutable not satisfied

;; RIGHT
(coll/mutate! posts nil {:title "X"})
```

### Field filtering in DataSource

```clojure
;; WRONG: DataSource strips fields — breaks pattern contract
(list-all [_]
  (d/q '[:find [(pull ?e [:id :title]) ...] :where [?e :id _]] @conn))

;; RIGHT: return complete entities, let patterns select shape
(list-all [_]
  (d/q '[:find [(pull ?e [*]) ...] :where [?e :id _]] @conn))
```

Format normalization (stripping `:db/id`, converting sets to vectors) is fine. Omitting domain fields is not. Ensure `fetch` and `list-all` return the same shape.

### Returning errors as exceptions from wrappers

```clojure
;; WRONG: throws — remote catches as :execution-error (500)
(throw (ex-info "Forbidden" {}))

;; RIGHT: return error as data — remote's :detect picks it up
{:error {:type :forbidden :message "Not the owner"}}
```

Remote's error config uses `:detect :error` to check mutation results for `:error` key, maps `:type` to HTTP status codes.

## Indexes

Queries must match a declared index or include the `id-key`:

```clojure
(coll/collection src {:id-key :post/id
                      :indexes #{#{:post/id} #{:post/author}}})

(get coll {:post/id 3})            ;; ✓ id-key fast path
(get coll {:post/author "alice"})  ;; ✓ matches declared index
(get coll {:post/title "Hello"})   ;; ✗ throws "No index for query"
```

Compound indexes work: `:indexes #{#{:status :type}}` allows `(get coll {:status :draft :type :blog})`.

## Quick Reference

### Protocols

| Protocol | Methods | Purpose |
|---|---|---|
| `DataSource` | `fetch`, `list-all`, `create!`, `update!`, `delete!` | Backend storage adapter |
| `Mutable` | `mutate!` | Unified CRUD: `(nil, data)` → create, `(query, data)` → update, `(query, nil)` → delete |
| `Wireable` | `->wire` | Serialize for Transit/EDN (collections → vectors, lookups → maps or nil) |
| `TxSource` | `snapshot`, `transact!` | Atomic batch mutations (atom-source only) |

### Constructors & Wrappers

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

All tests are inline RCT. When adding new DataSource implementations or wrappers, add RCT tests covering CRUD, edge cases (empty query, missing index), and Wireable output.
