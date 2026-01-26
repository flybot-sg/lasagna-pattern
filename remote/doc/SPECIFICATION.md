# Remote Pull Protocol Specification

**Version:** 0.2 (Draft)
**Status:** Draft for Review
**Authors:** Pullable Contributors

## 1. Introduction

Remote Pull is a declarative data access protocol that enables clients to request exactly the data they need using pattern-based queries. Unlike traditional RPC or REST, Remote Pull unifies reads and writes through a single pattern syntax where:

- **Literals** constrain/input data
- **Variables** extract/query data

This specification defines the wire format, protocol semantics, security model, and operational behavior.

### 1.1 Design Principles

1. **API = Lazy Data + Schema**: No resolver registry. The server exposes lazy data structures; computation happens on-demand.
2. **Pattern = Input + Query**: Single syntax for reads and writes. Patterns serve dual purpose.
3. **Transparent Mutations**: No separate mutation syntax. Client patterns are declarative.
4. **Session-Aware Access**: Schema introspection reflects current user's permissions.

### 1.2 Scope

This specification covers:
- Wire format and encoding
- HTTP transport bindings
- Pattern syntax for remote use
- CRUD operation semantics
- Error handling
- Security considerations

Out of scope:
- Pattern matching implementation (see `pattern` component)
- Collection abstractions (see `collection` component)
- Server-side lazy data construction

## 2. Wire Format

### 2.1 Content Types

| Format | Content-Type | Use Case |
|--------|-------------|----------|
| Transit+JSON | `application/transit+json` | Default, human-readable |
| Transit+MessagePack | `application/transit+msgpack` | Compact binary |
| EDN | `application/edn` | Clojure-native debugging |

Transit+JSON is the default format. Servers MUST support Transit+JSON; other formats are OPTIONAL.

### 2.2 Request Format

```clojure
{:pattern <pattern>       ;; REQUIRED - The pull pattern
 :params  <map>}          ;; OPTIONAL - Parameter substitutions
```

**Pattern**: An EDN data structure containing:
- Literals (constraints/inputs)
- Variables (`?name` symbols for extraction)
- Map/vector/sequence structures

**Params**: A map of keyword keys to values. Parameters substitute `$key` symbols in patterns:

```clojure
;; Request
{:pattern '{:user {:id $user-id :name ?name}}
 :params  {:user-id 123}}

;; Resolved pattern (server-side)
'{:user {:id 123 :name ?name}}
```

### 2.3 Response Format

#### Success Response

Success responses return the variable bindings directly as a map:

```clojure
{<symbol> <value>         ;; Symbol → value bindings from ?variables
 ...}
```

Example:
```clojure
{'name "Alice" 'email "alice@example.com"}
```

#### Error Response

```clojure
{:errors [<error>+]}
```

Where each `<error>` is:
```clojure
{:code   <keyword>        ;; Error classification
 :reason <string>         ;; Human-readable description
 :path   [<key>*]         ;; OPTIONAL - Location in pattern
 :value  <any>}           ;; OPTIONAL - Offending value
```

**Standard Error Codes:**

| Code | HTTP Status | Description |
|------|-------------|-------------|
| `:invalid-request` | 400 | Malformed request structure |
| `:decode-error` | 400 | Failed to decode request body |
| `:schema-violation` | 403 | Pattern requests disallowed key |
| `:binding-conflict` | 422 | Same variable bound to different values |
| `:match-failure` | 422 | Pattern failed to match data |
| `:invalid-collection` | 404 | Collection not found or unavailable |
| `:not-found` | 404 | Resource/endpoint not found |
| `:method-not-allowed` | 405 | Wrong HTTP method |
| `:execution-error` | 500 | Server error during pattern execution |

## 3. HTTP Transport

### 3.1 Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api` | POST | Execute pattern against API |
| `/api/_schema` | GET | Introspection - returns visible schema |

The base path `/api` is configurable.

### 3.2 Headers

**Request Headers:**
- `Content-Type`: Encoding of request body (default: `application/transit+json`)
- `Accept`: Preferred response encoding (default: `application/transit+json`)

**Response Headers:**
- `Content-Type`: Encoding of response body
- `Allow`: (405 only) Allowed methods for endpoint

### 3.3 Status Codes

| Status | Condition |
|--------|-----------|
| 200 | Successful operation |
| 400 | Malformed request (invalid syntax, decode error) |
| 403 | Forbidden (schema violation, access denied) |
| 404 | Not found (unknown endpoint, missing schema, invalid collection) |
| 405 | Wrong HTTP method |
| 422 | Unprocessable content (binding conflict, match failure) |
| 500 | Server error (execution error) |

### 3.4 Content Negotiation

1. Server parses `Content-Type` to determine request encoding
2. Server parses `Accept` to select response encoding
3. Unknown/missing headers default to Transit+JSON
4. `Accept: */*` defaults to Transit+JSON

## 4. Pattern Syntax

### 4.1 Variables

Variables are symbols prefixed with `?`:

| Syntax | Meaning |
|--------|---------|
| `?name` | Bind value to symbol `name` |
| `?_` | Wildcard - match but don't bind |
| `?name?` | Optional - zero or one |
| `?name*` | Zero or more |
| `?name+` | One or more |

### 4.2 Parameters

Parameters are symbols prefixed with `$`:

```clojure
;; Pattern with parameters
'{:user {:id $user-id :role $role}}

;; Params in request
{:user-id 123 :role "admin"}

;; Resolved
'{:user {:id 123 :role "admin"}}
```

Parameters support structural values (maps, vectors):

```clojure
'{:query $filter}  + {:filter {:status "active"}}
=> '{:query {:status "active"}}
```

### 4.3 Constraints

Pattern values can include constraints:

```clojure
{:age (?a :when pos?)}      ;; Predicate constraint
{:name (?n :default "N/A")} ;; Default on match failure
```

**Security Note**: Only whitelisted predicates are allowed in remote patterns. See Section 6.

### 4.4 Map Patterns

Map patterns match against map data:

```clojure
;; Pattern
{:user {:name ?name :email ?email}}

;; Data
{:user {:name "Alice" :email "alice@ex.com" :age 30}}

;; Result
{'name "Alice" 'email "alice@ex.com"}
```

Unmatched keys in data are preserved (passthrough semantics).

### 4.5 Indexed Lookup

Maps support indexed lookup via non-keyword keys:

```clojure
;; Pattern - lookup by query
{:posts {{:id 3} ?post}}

;; Equivalent to: (get posts {:id 3})
```

The query key is passed to the collection's `ILookup` implementation.

### 4.6 Sequence Patterns

```clojure
;; Match all items
{:items [?item*]}

;; Match first item
{:items [?first]}

;; Match with structure
{:orders [{:id ?id :total ?total}]}
```

## 5. CRUD Operations

Remote Pull supports CRUD through pattern syntax. The schema declares **nouns only** - no verb endpoints.

### 5.1 Schema Declaration

Schemas are defined using Malli, the Clojure schema library. Enable Malli integration by requiring `sg.flybot.pullable.malli`:

```clojure
(require '[malli.core :as m])
(require '[sg.flybot.pullable.malli])

;; Define entity schemas
(def post-schema
  (m/schema [:map
             [:id :int]
             [:title :string]
             [:content :string]]))

;; API schema uses plain maps at top level with Malli values
{:posts (m/schema [:or
                   [:vector post-schema]              ; LIST access
                   [:map-of {:id :int} post-schema]])} ; indexed lookup
```

The `:or` type declares the collection supports multiple access patterns:
- `[:vector post-schema]` - Sequential access (LIST)
- `[:map-of query-schema post-schema]` - Indexed lookup (GET by query)

### 5.2 Operation Patterns

| Operation | Pattern | Description |
|-----------|---------|-------------|
| **LIST** | `{:posts ?all}` | Variable binds whole collection |
| **READ** | `{:posts {{:id 3} ?post}}` | Query key + variable value |
| **CREATE** | `{:posts {nil {:title "Hi"}}}` | `nil` key + literal value |
| **UPDATE** | `{:posts {{:id 3} {:title "New"}}}` | Query key + literal value |
| **DELETE** | `{:posts {{:id 3} nil}}` | Query key + `nil` value |

### 5.3 Read vs Write Detection

**Rule**: Presence of any `?variable` in value position indicates a READ operation.

| Value | Operation |
|-------|-----------|
| `?post` | READ |
| `{:title "X"}` | WRITE |
| `{:title "X" :body ?b}` | **ERROR** (ambiguous) |

Mixed patterns (literals + variables in mutation position) are rejected.

### 5.4 Return Values

| Operation | Returns |
|-----------|---------|
| CREATE | Full created entity (with generated ID, timestamps) |
| UPDATE | Full updated entity |
| DELETE | `true` or the deleted entity |
| READ | Matched entity/entities |

### 5.5 Mutation Execution

Mutations are executed through the `Mutable` protocol:

```clojure
(mutate! collection query value)
```

Where:
- `query = nil` → CREATE
- `value = nil` → DELETE
- Both present → UPDATE

## 6. Security

### 6.1 Pattern Sandboxing

Remote patterns execute in a sandboxed environment:

1. **Predicate Whitelist**: Only type-checking predicates allowed in `:when` clauses
2. **No Code Evaluation**: `fn`/`fn*` forms are blocked
3. **Depth Limits**: Patterns cannot exceed 100 levels of nesting

### 6.2 Allowed Predicates

The following predicates are permitted in remote patterns:

```
string? number? integer? float? keyword? symbol? ident?
map? vector? list? set? seq? coll?
nil? some? true? false? boolean? empty?
pos? neg? zero? even? odd?
pos-int? neg-int? nat-int?
uuid? inst?
```

Any other symbol in predicate position causes an error.

### 6.3 Schema Enforcement

Patterns are compiled with schema validation:
- Keys not in schema are rejected
- Schema violations return `:schema-violation` errors
- Different sessions may have different schemas

### 6.4 Session Isolation

Each request executes against a fresh API instance created from the Ring request. Session data (user, permissions) determines:
- Which schema applies
- Which data is visible
- Which mutations are allowed

## 7. Introspection

### 7.1 Schema Endpoint

`GET /api/_schema` returns the schema visible to the current session:

```clojure
;; Admin sees
{:user {:id :number :name :string :email :string :role :keyword}
 :admin {:users [{:id :number :name :string}]}}

;; Regular user sees
{:user {:name :string :email :string}}
```

### 7.2 Schema Format

Schemas use Malli format. When serialized over the wire (via `Wireable` protocol), Malli schemas convert to their EDN form:

| Wire Format | Malli Type | Meaning |
|-------------|------------|---------|
| `:string` | `:string` | String value |
| `:int`, `:double` | `:int`, `:double` | Numeric values |
| `:keyword` | `:keyword` | Keyword value |
| `:boolean` | `:boolean` | Boolean value |
| `:any` | `:any` | Any value |
| `[:vector schema]` | `:vector` | Sequence of schema |
| `[:map [:k schema] ...]` | `:map` | Map with typed keys |
| `[:or ...]` | `:or` | Union of schemas |
| `[:enum ...]` | `:enum` | Enum (set of allowed values) |
| `[:maybe schema]` | `:maybe` | Optional/nullable |

Plain Clojure maps `{:k schema}` are also supported as shorthand for record types.

### 7.3 Schema Documentation

Schema documentation uses Malli's hiccup syntax with inline properties on each field entry. This is the idiomatic Malli approach where documentation lives alongside the schema definition.

#### Property Keys

**Map-level properties:**

| Key | Type | Description |
|-----|------|-------------|
| `:version` | string | Schema version for client compatibility and cache invalidation |
| `:doc` | string | Human-readable description of the schema |

**Field-level properties:**

| Key | Type | Description |
|-----|------|-------------|
| `:doc` | string | Human-readable description of the field |
| `:deprecated` | boolean/string | Mark as deprecated, optionally with migration note |
| `:example` | any | Example value |
| `:since` | string | Version when field was added |
| `:optional` | boolean | Mark field as optional (Malli built-in) |

**Collection-level properties (on `:vector`, `:sequential`, etc.):**

| Key | Type | Description |
|-----|------|-------------|
| `:ilookup` | boolean | Enables indexed lookup patterns `{{:query} ?result}` |
| `:operations` | map | Documents CRUD operations (for tooling/introspection) |

#### Map Schema Documentation

Use inline properties on each field entry in the Malli hiccup syntax:

```clojure
(require '[malli.core :as m])

(def user-schema
  (m/schema
    [:map {:doc "User account information"}
     [:id {:doc "Unique identifier" :example 12345} :int]
     [:name {:doc "Display name"} :string]
     [:email {:doc "Primary email address"} :string]
     [:role {:doc "User role" :example :admin} [:enum :admin :editor :viewer]]
     [:legacy {:doc "Legacy role field" :deprecated "Use :role instead" :optional true} :string]]))
```

#### Sequence Schema Documentation

For vector schemas, put documentation on the map properties:

```clojure
(def posts-schema
  (m/schema
    [:vector {:doc "Published blog posts, ordered by date descending"}
     [:map
      [:id {:doc "Post identifier"} :int]
      [:title {:doc "Post title"} :string]
      [:content {:doc "Post content"} :string]]]))
```

#### Collection Schema Documentation

For collections with ILookup support (supporting both LIST and GET), use `:ilookup true` to enable indexed lookup pattern validation:

```clojure
(def posts-collection-schema
  (m/schema
    [:vector {:ilookup true
              :doc "Blog post collection"
              :operations {:list   "Returns all posts"
                           :get    "Lookup by {:id n}"
                           :create "Provide {:title :content}"
                           :update "Partial updates supported"
                           :delete "Returns deleted post"}}
     post-schema]))
```

The `:ilookup` property tells the pattern compiler that this collection supports indexed lookup patterns like `{{:id 1} ?post}`. Without this annotation, such patterns will be rejected at compile time with a helpful error message.

| Property | Type | Description |
|----------|------|-------------|
| `:ilookup` | boolean | Enables indexed lookup patterns `{{:query} ?result}` |
| `:operations` | map | Documents CRUD operations (for tooling/introspection) |

#### Nested Schema Documentation

For nested maps, documentation is inline at each level:

```clojure
(def api-schema
  (m/schema
    [:map {:version "2.0.0" :doc "API root"}
     [:user {:doc "Current user profile"}
      [:map
       [:name {:doc "Display name"} :string]
       [:email {:doc "Primary email"} :string]]]
     [:posts {:doc "Blog posts collection"}
      posts-collection-schema]]))
```

Note: API root schemas can also be plain Clojure maps (not Malli schemas) to allow middleware to `assoc` additional keys like `:me` for authentication. In that case, use Clojure metadata for root-level properties.

#### Introspection Response

`GET /api/_schema` returns schema with inline documentation:

```clojure
[:map {:version "1.2.0" :doc "User API"}
 [:user {:doc "Current user profile"}
  [:map
   [:name {:doc "Display name"} :string]
   [:email {:doc "Primary email" :since "1.1.0"} :string]]]]
```

Clients can use `:version` for compatibility checking and cache invalidation.

#### Wire Format Considerations

The hiccup schema format serializes naturally to Transit and EDN. Documentation properties are inline with each field entry.

Clients that don't need documentation can ignore the properties maps - the structural schema remains valid.

## 8. Request Flow

```
┌─────────────────────────────────────────────────────────────┐
│                        Client                               │
└─────────────────────────┬───────────────────────────────────┘
                          │ POST /api
                          │ {:pattern ... :params ...}
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                    Ring Handler                             │
│  1. Parse Content-Type → decode request                     │
│  2. Negotiate Accept → select response format               │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                  Param Resolution                           │
│  resolve-params(pattern, params) → resolved pattern         │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                 Security Validation                         │
│  1. Validate pattern depth ≤ 100                            │
│  2. Verify predicates in whitelist                          │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                  Mutation Detection                         │
│  parse-mutation(pattern) → mutation | nil                   │
└──────────────┬──────────────────────────────┬───────────────┘
               │ mutation                      │ nil (read)
               ▼                               ▼
┌──────────────────────────┐    ┌─────────────────────────────┐
│   Execute Mutation       │    │   Execute Pull Pattern      │
│   mutate!(coll, q, v)    │    │   1. api-fn(req) → data     │
└──────────────┬───────────┘    │   2. compile(pattern,       │
               │                │        {:schema, :resolve}) │
               │                │   3. match(data) → result   │
               │                └──────────────┬──────────────┘
               │                               │
               └───────────────┬───────────────┘
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                   Response Building                         │
│  success: {<symbol> <value> ...}  (vars bindings)           │
│  failure: {:errors [...]}                                   │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                    Encode & Return                          │
│  encode(response, format) → Ring response                   │
└─────────────────────────────────────────────────────────────┘
```

## 9. Examples

### 9.1 Simple Query

**Request:**
```clojure
{:pattern '{:user {:name ?name :email ?email}}}
```

**Response:**
```clojure
{'name "Alice" 'email "alice@example.com"}
```

### 9.2 Parameterized Query

**Request:**
```clojure
{:pattern '{:posts {{:id $id} ?post}}
 :params  {:id 42}}
```

**Response:**
```clojure
{'post {:id 42 :title "Hello World" :author "Alice"}}
```

### 9.3 Create Operation

**Request:**
```clojure
{:pattern '{:posts {nil {:title "New Post" :content "..."}}}}
```

**Response:**
```clojure
{'posts {:id 100 :title "New Post" :content "..." :created-at #inst "2025-01-25"}}
```

### 9.4 Update Operation

**Request:**
```clojure
{:pattern '{:posts {{:id 42} {:title "Updated Title"}}}}
```

**Response:**
```clojure
{'posts {:id 42 :title "Updated Title" :content "..." :updated-at #inst "2025-01-25"}}
```

### 9.5 Delete Operation

**Request:**
```clojure
{:pattern '{:posts {{:id 42} nil}}}
```

**Response:**
```clojure
{'posts true}
```

### 9.6 Schema Violation

**Request:**
```clojure
{:pattern '{:user {:password ?pw}}}  ;; :password not in schema
```

**Response:**
```clojure
{:errors [{:code :schema-violation
           :reason "key not in schema"
           :path [:user :password]}]}
```

## 10. Conformance

### 10.1 Server Requirements

A conforming server MUST:
- Support Transit+JSON encoding
- Implement `POST /api` for pattern execution
- Implement `GET /api/_schema` for introspection
- Validate patterns against schema
- Sandbox pattern evaluation (no arbitrary code execution)
- Return proper error responses with error codes

A conforming server SHOULD:
- Support Transit+MessagePack encoding
- Support EDN encoding
- Implement pattern depth limits

### 10.2 Client Requirements

A conforming client MUST:
- Send valid pattern structures
- Handle both success and error responses
- Use appropriate Content-Type headers

## Appendix A: Grammar

```ebnf
request     = "{" ":pattern" pattern [":params" params] "}"
pattern     = map-pat | seq-pat | variable | literal | param
map-pat     = "{" (key pattern)* "}"
seq-pat     = "[" pattern* "]"
variable    = "?" name ["?" | "*" | "+"]
param       = "$" name
key         = keyword | map-pat
literal     = string | number | keyword | boolean | nil | map | vector
params      = "{" (keyword literal)* "}"
name        = symbol-chars
```

## Appendix B: Changelog

### v0.2 (Draft)
- Added parameter substitution ($params)
- Added security sandboxing (predicate whitelist, depth limits)
- Added mutation detection and CRUD semantics
- Clarified read/write discrimination rule
- Added schema documentation via metadata (:doc, :fields, :deprecated, :example, :since)

### v0.1
- Initial protocol design
- Basic request/response format
- Schema introspection
