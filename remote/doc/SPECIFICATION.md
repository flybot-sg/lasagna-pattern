# Remote Pull Protocol Specification

Reference specification for the remote pull wire protocol. For usage and API docs, see the [README](../README.md). For architecture and implementation details, see [CLAUDE.md](../CLAUDE.md).

## Error Codes

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

Custom error codes from the `:errors` config (e.g., `:forbidden`, `:not-found`) are mapped to HTTP status via the `:codes` map in the api-fn response.

## HTTP Status Codes

| Status | Condition |
|--------|-----------|
| 200 | Successful operation |
| 400 | Malformed request (invalid syntax, decode error) |
| 403 | Forbidden (schema violation, access denied) |
| 404 | Not found (unknown endpoint, missing schema, invalid collection) |
| 405 | Wrong HTTP method |
| 422 | Unprocessable content (binding conflict, match failure) |
| 500 | Server error (execution error) |

## Headers

**Request:**
- `Content-Type`: Encoding of request body (default: `application/transit+json`)
- `Accept`: Preferred response encoding (default: `application/transit+json`)

**Response:**
- `Content-Type`: Encoding of response body
- `Allow`: (405 only) Allowed methods for endpoint

## Schema Documentation

Schema documentation uses Malli's hiccup syntax with inline properties on each field entry.

### Property Keys

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

### Examples

Map schema with field docs:

```clojure
(def user-schema
  (m/schema
    [:map {:doc "User account information"}
     [:id {:doc "Unique identifier" :example 12345} :int]
     [:name {:doc "Display name"} :string]
     [:email {:doc "Primary email address"} :string]
     [:role {:doc "User role" :example :admin} [:enum :admin :editor :viewer]]
     [:legacy {:doc "Legacy role field" :deprecated "Use :role instead" :optional true} :string]]))
```

Collection schema with ILookup and operation docs:

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

The `:ilookup` property tells the pattern compiler that this collection supports indexed lookup patterns like `{{:id 1} ?post}`. Without this annotation, such patterns are rejected at compile time.

Nested schema:

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

API root schemas can also be plain Clojure maps (not Malli schemas) to allow middleware to `assoc` additional keys. In that case, use Clojure metadata for root-level properties.

### Schema Wire Format

Malli schemas serialize naturally to Transit and EDN:

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

Documentation properties are inline with each field entry. Clients that don't need documentation can ignore the properties maps.

## Grammar

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
