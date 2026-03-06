# Remote

HTTP transport for pull-based pattern APIs. Turns `pattern` + `collection` into a GraphQL-like protocol without schema languages or resolver registries.

## Why This Component Exists

`pattern` matches data. `collection` makes data mutable. **Remote closes the loop** — it sends patterns over HTTP and returns results, making the entire read/write cycle a single round-trip.

Without remote, you'd need to build REST endpoints, mutation handlers, serialization, error mapping, and content negotiation by hand. Remote does all of this by interpreting the pattern syntax that `pattern` and `collection` already understand.

## Critical: Mutations Are Round-Trips

**This is the most important concept in the entire toolbox.**

A mutation pattern is still a pull — it returns the result of the mutation in the same response. The chain:

```
Client sends:  {:posts {nil {:title "New"}}}        ; CREATE pattern
                        ↓
Remote calls:  (coll/mutate! posts nil {:title "New"})
                        ↓
DataSource:    create! returns full entity {:post/id 42 :title "New" :created-at ...}
                        ↓
Remote sends:  {posts {:post/id 42 :title "New" :created-at ...}}   ; response
                        ↓
Client uses:   (get response 'posts)  → the created entity
```

| Operation | Pattern | Response |
|-----------|---------|----------|
| CREATE | `{:posts {nil {:title "New"}}}` | `{posts {:post/id 42 :title "New" ...}}` |
| UPDATE | `{:posts {{:post/id 42} {:title "X"}}}` | `{posts {:post/id 42 :title "X" ...}}` |
| DELETE | `{:posts {{:post/id 42} nil}}` | `{posts true}` |
| READ | `'{:posts ?all}` | `{posts [{...} {...}]}` |

The pattern describes the *change*, the response gives back the *result*. Update just the title or just the content — the response is always the complete entity. This is guaranteed by the `collection` contract (`DataSource.update!` returns the full entity) and `remote` passes it through unchanged. Application code does not need to test this.

**Anti-pattern: discarding the mutation response and re-fetching.** The pull pattern system exists so that every interaction (read or write) returns data. Ignoring the response and making a second request to re-fetch defeats the entire design.

## Core Design

```
Client Pattern (EDN over Transit)
    │
    ▼
Ring Handler (make-handler / wrap-api)
    │
    ├── Content negotiation (Transit/EDN)
    ├── Parameter substitution ($params)
    ├── Security validation (depth, predicates)
    │
    ├── parse-mutation(pattern) → mutation | nil
    │         │                        │
    │    MUTATION PATH            READ PATH
    │    detect-path-error       detect-read-errors
    │      (errors along path)     (walk var paths w/ :detect)
    │    Walk path to find       Trim pattern at error paths
    │      the collection        Compile trimmed pattern
    │    mutate!(coll, q, v)     Match against original data
    │    detect errors           Classify: partial success
    │    Return full entity        or full failure
    │         │                        │
    │         └────────┬───────────────┘
    │                  ▼
    ├── prepare-for-wire (Wireable → serializable)
    └── encode response → Ring response
```

**Key**: `pattern` (match-fn) is only used on the READ path. On the MUTATION path, `remote` walks the pattern path itself and calls `coll/mutate!` directly — `pattern` is not involved.

## Public API

### Server (`sg.flybot.pullable.remote`)

| Function | Description |
|----------|-------------|
| `make-handler` | Create Ring handler from api-fn |
| `wrap-api` | Ring middleware adding pull API at path |
| `parse-mutation` | Detect mutation in pattern → `{:path :query :value}` or nil |
| `execute` | Execute pattern directly (no HTTP) — for in-process callers like sandboxes |
| `encode` / `decode` | Transit/EDN serialization |

### Client (`sg.flybot.pullable.remote.client`)

| Function | Description |
|----------|-------------|
| `connect` | Create client function for endpoint (JVM only) |
| `schema` | Fetch schema introspection |
| `url` | Get endpoint URL |

## API Function Contract

The function passed to `make-handler` receives a Ring request and returns:

```clojure
{:data   {...}     ; Map of collections/lookups (ILookup + Mutable)
 :schema {...}     ; Malli schemas per top-level key (for validation)
 :errors {:detect :error              ; keyword or fn to detect errors
          :codes {:forbidden 403}}    ; error type → HTTP status
 :sample {...}}    ; Sample data for GET /_schema
```

## Mutation Detection

`parse-mutation` determines read vs write by inspecting the pattern:

- **Variables** (`?x`) in value position → READ
- **Literals** in value position → WRITE
- **Mixed** (variables + literals) → ERROR (ambiguous)

```clojure
;; Flat patterns
{:posts '?all}                    ; nil → READ
{:posts {nil {:title "X"}}}       ; {:path [:posts] :query nil :value {...}} → CREATE
{:posts {{:id 1} {:title "X"}}}   ; {:path [:posts] :query {:id 1} :value {...}} → UPDATE
{:posts {{:id 1} nil}}            ; {:path [:posts] :query {:id 1} :value nil} → DELETE

;; Nested patterns (role-based)
{:member {:posts {nil data}}}     ; {:path [:member :posts] :query nil :value data} → CREATE
```

## Wire Protocol

**Endpoints:**
- `POST /api` — Execute pull pattern
- `GET /api/_schema` — Schema introspection. Returns `{:schema {...} :sample {...}}` where `:schema` is the Malli schema map and `:sample` is handcrafted example data. Both keys are optional (`:sample` omitted if api-fn doesn't provide one).

**Content negotiation** via Accept/Content-Type:
- `application/transit+json` (default)
- `application/transit+msgpack`
- `application/edn`

**Request:** `{:pattern '{:posts ?all} :params {:limit 10}}`
**Success:** `{'all [{...} {...}]}`
**Error:** `{:errors [{:code :forbidden :reason "..." :path [:member :posts]}]}`

## Error Handling

Collections return errors as data (not exceptions):

```clojure
;; Collection returns:
{:error {:type :forbidden :message "You don't own this post"}}

;; Error config in api-fn:
{:detect :error                ; keyword or fn to detect errors in data
 :codes  {:forbidden 403       ; error type → HTTP status
          :not-found 404}}
```

**Mutations** are all-or-nothing: Remote checks the mutation result with `:detect`, maps `:type` to HTTP status via `:codes`. Path-level errors (e.g., role gate returning `{:error ...}` along the path) are detected before attempting the mutation.

**Reads** support partial success: Before pattern matching, `execute-read` extracts var paths from the pattern and walks each path through the data (including through ILookup) checking for errors via `:detect`. The pattern is trimmed to remove error paths, and matching proceeds on the original data. If some branches succeed and others fail:
- Successful bindings are returned normally
- Detected errors are attached as `::detected-errors` metadata and included in the wire response as `:errors`

If all pattern paths are error paths, the read fails with the full error list.

**Partial success applies to reads only.** Mutations remain all-or-nothing.

## Security

- **Predicate whitelist**: Only safe type-checking predicates in `:when` clauses
- **No code evaluation**: `fn`/`fn*` forms blocked
- **Depth limits**: Patterns capped at 100 levels of nesting
- **Schema enforcement**: Keys not in schema rejected with `:schema-violation`

## Testing

```bash
bb test remote
```

Tests are inline RCT in source files.
