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
    └── encode response → Ring response
        (Wireable → serializable handled in `success` via normalize-value)
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
 :errors {:detect :error              ; keyword or fn → must return nil or {:type _ :message _}
          :codes {:forbidden 403}}    ; error type → HTTP status
 :sample {...}}    ; Sample data for GET /_schema
```

## Mutation Detection

`parse-mutation` determines read vs write by inspecting the pattern:

- **Variables** (`?x`) in value position → READ
- **Literals** in value position → WRITE
- **Mixed** (variables + literals) → ERROR (ambiguous)

**Constraints:** One pattern = one operation type. You cannot mix reads and writes in a single pattern, and only one mutation per pattern is supported. Clients needing both a write and a read must send two separate requests.

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

**`:detect` contract:** The detect function (or the value at the detect keyword) must return `nil` for no error, or a map with `:type` (keyword) and optional `:message` (string). Non-map truthy returns (e.g., strings, booleans) violate the contract and will cause runtime errors.

**Mutations** are all-or-nothing: Remote checks the mutation result with `:detect`, maps `:type` to HTTP status via `:codes`. Path-level errors (e.g., role gate returning `{:error ...}` along the path) are detected before attempting the mutation.

**Reads** support partial success via three phases. Each ILookup is invoked at most once:

1. **Pre-match walk** of plain-map data along pattern var-paths. Collects auth errors upfront and trims those branches from the pattern. Walk stops at any non-map value — ILookup implementations are **not** probed. This handles role-gate denials where the pattern nests past the error level (e.g., `{:member {:posts/history {{:post/id 1} ?v}}}` against `{:member {:error ...}}`).
2. **Match** on the trimmed pattern. Untrimmed branches see the original data; each accessed ILookup key fires exactly once.
3. **Post-match walk** of the matcher's `:val` for errors inside materialized data (leaf errors, errors deep inside ILookup returns). Skips paths already covered by pre-match errors.

Pre- and post-errors merge. If all pattern paths are error-covered, the read fails; otherwise it returns the successful bindings with `::detected-errors` metadata (exposed on the wire as `:errors`).

**Error-data contract: errors must live in plain maps.** `{:error {:type ... :message ...}}` inside plain data is detected by pre-walk and post-walk alike. Errors returned from `ILookup.valAt` are invisible to pre-walk (it stops at ILookup boundaries) and to post-walk on match failure (`:val` is nil). If a pattern descends past such an error, the response is `:match-failure` (422), not the domain error.

In practice, produce `{:error ...}` at role-gate points (e.g., flybot-site's `with-role`) as plain data, not from inside `valAt`.

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
