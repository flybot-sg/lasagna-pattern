# Remote Protocol Design

This document captures the design philosophy and decisions for the `remote` component, which extends pull patterns into a remote protocol over HTTP.

## Philosophy

### API = Lazy Data + Schema

Unlike GraphQL which separates schema definition from resolver implementation, our approach unifies them:

```
GraphQL:  Query → Resolver Functions → Data → Response
Pull:     Pattern → Lazy Data Structure → Response
```

The "resolver" logic lives **inside** the lazy data structure itself. When a pattern accesses a key, the lazy evaluation triggers computation. No separate resolver registry needed.

```clojure
(require '[robertluo.fun-map :refer [fnk life-cycle-map]])

(defn make-api [session]
  (life-cycle-map
    {:user     (fnk [] (db/get-user (:user-id session)))
     :orders   (fnk [user] (db/get-orders (:id user)))
     :products (fnk [] (lazy-seq (db/list-products)))}))
```

Benefits:
- **Laziness** = demand-driven computation (no over-fetching)
- **Schema** = access control + contract
- **Pattern** = declarative selection

### Pattern = Input + Query (Unification Model)

A pattern serves **dual purpose**:

```clojure
{:operator :add        ; ← INPUT (literal constraint)
 :operands [3 5]       ; ← INPUT (literal values)
 :result   ?result}    ; ← QUERY (extract this)
```

The client doesn't think "I'm calling a function" or "I'm mutating data". They declare:

> "Give me a world where operator is :add, operands are [3,5], and tell me what result is."

This is **declarative unification**, not imperative RPC. The distinction between reads and writes becomes transparent to the client.

### Mutations are Transparent

There is no separate mutation syntax. Whether a pattern triggers a database write, a pure computation, or a cache lookup is an implementation detail hidden from the client.

Example - a calculator API:

```clojure
;; Schema
{:operator #{:add :subtract :multiply}
 :operands [:number]
 :result   :number}

;; Server-side lazy data structure
(defn make-calculator-api [request-data]
  (life-cycle-map
    {:operator (:operator request-data)
     :operands (:operands request-data)
     :result   (fnk [operator operands]
                 (case operator
                   :add      (apply + operands)
                   :subtract (apply - operands)
                   :multiply (apply * operands)))}))

;; Client pattern (acts as both input AND query)
{:operator :add
 :operands [3 5]
 :result   ?result}
;; => {:result 8, 'result 8}
```

### Schema as Visibility Control

Schema serves dual purpose:
1. **Validation** - Pattern can only request fields declared in schema
2. **Introspection** - Schema IS the API documentation

Different users see different schemas based on permissions. The schema attached to the data structure controls what's accessible.

## Wire Format

### Primary Format: EDN/Transit

The library targets Clojure ecosystem. EDN is the canonical format, Transit for efficiency over the wire.

For non-Clojure clients, we recommend developing EDN notation libraries for target languages rather than compromising the protocol with JSON limitations.

### Request Shape

```clojure
{:pattern '{:user {:name ?n :email ?e}
            :orders [{:total ?t}]}
 :params  {:limit 10}}  ; optional, for server-side use
```

### Response Shape

Success:
```clojure
{:data {:user {:name "Alice" :email "alice@example.com"}
        :orders [{:total 99.00} {:total 45.50}]}
 :vars {'n "Alice"
        'e "alice@example.com"
        't [99.00 45.50]}}
```

Failure:
```clojure
{:errors [{:path [:user :email]
           :reason "key not in schema"
           :code :schema-violation}]}
```

## HTTP Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api` | POST | Execute pattern against API |
| `/api/_schema` | GET | Introspection - returns schema visible to current session |

All requests use Transit encoding (application/transit+json or application/transit+msgpack).

## Introspection

The schema endpoint is **session-aware**. Different users see different APIs based on their permissions:

```clojure
;; Admin sees:
{:user {:id :number :name :string :email :string :role :keyword}
 :orders [:map-of :keyword :any]
 :admin {:users [:seq {:id :number :name :string}]}}

;; Regular user sees:
{:user {:name :string :email :string}
 :orders [{:id :number :total :number}]}
```

The schema returned is the **actual schema** that will be enforced when processing patterns.

## Request Flow

```
┌─────────┐    Transit     ┌─────────────┐
│ Client  │ ──────────────→│ Ring Handler│
│         │    Pattern     │             │
└─────────┘                └──────┬──────┘
                                  │
                    ┌─────────────▼─────────────┐
                    │ api-fn(request) → lazy-map │
                    │ with schema metadata       │
                    └─────────────┬─────────────┘
                                  │
                    ┌─────────────▼─────────────┐
                    │ compile-pattern(pattern,   │
                    │   {:schema (meta lazy-map)})│
                    └─────────────┬─────────────┘
                                  │
                    ┌─────────────▼─────────────┐
                    │ match pattern against      │
                    │ lazy-map → triggers only   │
                    │ needed computations        │
                    └─────────────┬─────────────┘
                                  │
                    ┌─────────────▼─────────────┐
                    │ {:data ... :vars ...}      │
                    │ or {:errors [...]}         │
                    └───────────────────────────┘
```

## Scope

### v0.1 (Initial Release)

| Feature | Description |
|---------|-------------|
| Ring handler | `(make-handler api-fn)` where `api-fn`: request → lazy data with schema |
| Schema introspection | `GET /api/_schema` returns session-aware schema |
| Transit encoding | Request/response serialization |
| Error mapping | MatchFailure → HTTP error response |

### Deferred (Future Versions)

| Feature | Notes |
|---------|-------|
| Pattern parameterization | May belong in `pattern` library; clients can construct complete patterns for now |
| WebSocket/SSE subscriptions | Live data updates |
| Query complexity limits | DoS protection for public APIs |
| Caching hints | ETags, cache-control based on pattern |

## Comparison with Alternatives

| Aspect | Pull Remote | GraphQL | REST |
|--------|-------------|---------|------|
| Schema definition | Clojure maps + pattern DSL | SDL | OpenAPI |
| Query language | EDN patterns | GraphQL query language | URL params / JSON |
| Resolver model | Lazy data structures | Resolver functions | Controller methods |
| Mutation syntax | Transparent (same as query) | Explicit `mutation` | HTTP verbs (POST/PUT/DELETE) |
| Type safety | Schema validation at compile | Type system | Optional |
| N+1 problem | Lazy sequences + fun-map deps | DataLoader | Eager loading / includes |
| Learning curve | Low for Clojure devs | Medium | Low |

## Design Decisions Log

### 2024-01: Initial Design

1. **EDN/Transit over JSON**: Target Clojure ecosystem; other languages can use EDN libraries.

2. **No resolver registry**: Use lazy data structures (fun-map, lazy-seq) instead of GraphQL-style resolvers. Verbs become nouns through laziness.

3. **Transparent mutations**: No separate mutation syntax. Client patterns are declarative; whether they trigger reads or writes is an implementation detail.

4. **Session-aware introspection**: Schema endpoint returns only what current user can access. Different permissions = different visible API.

5. **Defer parameterization**: Keep remote layer simple. Pattern parameterization (template patterns with `$variable` holes) may belong in `pattern` library. For v0.1, clients construct complete patterns.

### 2025-01: CRUD Protocol Best Practices

A cohesive model for CRUD operations using noun-only schemas and unified access patterns.

#### Core Principle: Schema as Single Source of Truth

Schema contains **only nouns** - no verbs like `create-post` or `delete-post`:

```clojure
;; Good: noun-only
{:posts [post-schema]}

;; Bad: verbs mixed in
{:posts [post-schema]
 :create-post post-schema    ; ← verb
 :delete-post :boolean}      ; ← verb
```

#### Collection Access via ILookup

Collections support both sequential access and indexed lookup through ILookup interface:

```clojure
;; Schema declares union type for explicit capability
{:posts (union [post-schema] {post-query post-schema})}

;; Or with Malli schemas, use :ilookup property:
[:map
 [:posts [:vector {:ilookup true}
          [:map [:id :int] [:title :string]]]]]
```

| Pattern | Operation | Description |
|---------|-----------|-------------|
| `{:posts ?all}` | LIST | Bind whole collection |
| `{:posts {{:id 3} ?post}}` | GET | ILookup with query key |

**Note:** When using Malli schemas, the `:ilookup true` property on vector/sequential types explicitly enables indexed lookup patterns. Without this annotation, pattern compilation rejects map patterns on sequence schemas with a helpful error message.

#### CRUD Operations via Pattern Syntax

| Pattern | Operation | Rule |
|---------|-----------|------|
| `{:posts ?all}` | LIST | Variable binds collection |
| `{:posts {{:id 3} ?post}}` | READ | Query key + variable value |
| `{:posts {nil {:title "Hi" ...}}}` | CREATE | nil key + literal value |
| `{:posts {{:id 3} {:title "New" ...}}}` | UPDATE | Query key + literal value |
| `{:posts {{:id 3} nil}}` | DELETE | Query key + nil value |

#### Read/Write Discrimination Rule

**Presence of any variable in value position → READ operation**

| Value Position | Intent | Valid? |
|----------------|--------|--------|
| `?post` | READ | ✓ |
| `{:title "X" :content "Y"}` | WRITE | ✓ |
| `{:title "X" :content ?c}` | Mixed | **ERROR** |

Mixed patterns (literals + variables) are **rejected** as ambiguous. Rationale:
1. CRUD operations have real consequences; explicit intent prevents accidents
2. Write operations return the full mutated entity anyway
3. Simple mental model: "Variables = read, Literals = write, Mixed = error"

#### ILookup is Backend-Specific

The query key (e.g., `{:id 3}`) is passed to backend's ILookup implementation, which:

1. Interprets the query (translate to SQL WHERE, memory scan, API call, etc.)
2. Validates query is supported (e.g., reject if no index on queried field)
3. Returns result or error

```
Pattern                      Backend ILookup              Execution
──────────────────────────────────────────────────────────────────────
{:posts {{:id 3} ?p}}    →  posts.get({:id 3})     →  SELECT * WHERE id=3
{:posts {{:author "X"} ?p}} →  posts.get({:author "X"}) →  ERROR (no index)
                                                          or query if indexed
```

#### Return Values

| Operation | Returns |
|-----------|---------|
| CREATE | Full created entity (with generated `:id`, timestamps, etc.) |
| UPDATE | Full updated entity |
| DELETE | `true` on success, or deleted entity |
| READ | Matched entity/entities |

## Open Questions

1. **Partial success semantics**: When pattern partially matches (some fields succeed, some fail due to schema), should we return partial data with errors, or fail the entire request?

2. **Streaming responses**: For large lazy sequences, should we support streaming/pagination at protocol level, or leave it to schema design (e.g., `:limit`, `:offset` as pattern inputs)?

3. **Schema format for introspection**: Return our schema DSL directly, or provide a more structured/normalized format?
