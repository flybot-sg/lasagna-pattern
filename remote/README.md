# remote

[![lasagna-remote](https://img.shields.io/clojars/v/sg.flybot/lasagna-remote.svg)](https://clojars.org/sg.flybot/lasagna-remote) ![CI](https://github.com/flybot-sg/lasagna-pattern/actions/workflows/ci.yml/badge.svg) ![License: Unlicense](https://img.shields.io/badge/license-Unlicense-blue.svg)

Part of the [Lasagna Pattern](https://github.com/flybot-sg/lasagna-pattern) toolbox.

Remote protocol for pattern-based APIs (GraphQL-like) over HTTP.

## Rationale

Building APIs typically requires defining schemas, writing resolvers, and maintaining separate query languages. This library lets you **send patterns directly over HTTP**:

- **No schema language** - Patterns are EDN, same as your Clojure code
- **Unified CRUD** - Same pattern syntax for queries and mutations
- **Thin transport** - Just serializes patterns and results
- **Client included** - Connect and query with a single function (JVM only)

## Installation

```clojure
;; deps.edn
{:deps {sg.flybot/lasagna-remote {:mvn/version "0.1.0"}}}

;; Leiningen
[sg.flybot/lasagna-remote "0.1.0"]
```

## Usage

### Server

```clojure
(require '[sg.flybot.pullable.remote :as remote])

;; Define API function: ring-request → {:data :schema :errors :sample}
(defn my-api [ring-request]
  {:data   {:posts posts-collection
            :users users-collection}
   :schema {:posts [:vector Post]
            :users [:vector User]}
   :errors {:detect :error
            :codes  {:forbidden 403
                     :not-found 404}}})

;; Create Ring handler
(def handler (remote/make-handler my-api))

;; Or use as middleware on existing handler
(def app (-> other-handler
             (remote/wrap-api my-api {:path "/api"})))
```

### Client (JVM only)

```clojure
(require '[sg.flybot.pullable.remote.client :as client])

(def api (client/connect "http://localhost:8080/api"))

;; Pull patterns over HTTP
(api '{:posts ?all})
(api '{:posts {{:id 1} ?post}})
(api {:posts {nil {:title "New Post"}}})  ; mutations

;; Introspect schema
(client/schema api)
```

## API Function Contract

The function passed to `make-handler`/`wrap-api` receives a Ring request and returns:

```clojure
{:data   {...}     ; Map of collections/data (required)
 :schema {...}     ; Malli schema for validation (optional)
 :errors {...}     ; Error handling config (optional)
 :sample {...}}    ; Sample data for GET /_schema (optional)
```

### Error Handling

Collections return errors as data: `{:error {:type :forbidden :message "..."}}`

The `:errors` config tells the handler how to detect and translate these:

```clojure
{:detect :error           ; keyword: (get result :error)
                          ; or fn: (fn [result] error-map-or-nil)
 :codes {:forbidden 403   ; Map error :type to HTTP status
         :not-found 404
         :invalid   422}}
```

Example from flybot-site:

```clojure
(def error-config
  {:detect :error
   :codes {:forbidden 403
           :not-found 404
           :invalid-mutation 422}})
```

## Wire Protocol

**Endpoints:**
- `POST /api` - Execute pull pattern
- `GET /api/_schema` - Schema introspection

**Content negotiation** via Accept/Content-Type:
- `application/transit+json` (default)
- `application/transit+msgpack`
- `application/edn`

**Request format:**
```clojure
{:pattern '{:posts ?all}
 :params {:user-id 123}}  ; optional, replaces $user-id in pattern
```

**Response format:**
```clojure
;; Success: just the bindings
{'all [{:id 1 :title "Hello"}]}

;; Error
{:errors [{:code :forbidden :reason "You don't own this post"}]}
```

## Pattern Syntax

Same as [pattern](../pattern) DSL, sent over the wire:

```clojure
'{:posts ?all}                           ; List
'{:posts {{:id 1} ?post}}                ; Read by key
{:posts {nil {:title "New"}}}            ; Create
{:posts {{:id 1} {:title "Updated"}}}    ; Update
{:posts {{:id 1} nil}}                   ; Delete
```

## Public API

### Server (`sg.flybot.pullable.remote`)

| Function | Signature | Description |
|----------|-----------|-------------|
| `make-handler` | `[api-fn]` or `[api-fn opts]` | Create Ring handler for pull API |
| `wrap-api` | `[handler api-fn]` or `[handler api-fn opts]` | Ring middleware that adds pull API |
| `encode` | `[value format]` | Encode to bytes (for custom clients) |
| `decode` | `[bytes format]` | Decode from bytes (for custom clients) |

Options: `{:path "/api"}` (default path)

### Client (`sg.flybot.pullable.remote.client`)

| Function | Signature | Description |
|----------|-----------|-------------|
| `connect` | `[url]` or `[url opts]` | Create client function for endpoint |
| `schema` | `[api-fn]` | Fetch schema from connected API |
| `url` | `[api-fn]` | Get URL of connected API |

Options: `{:format :transit-json, :client http-client}`

## Development

All commands are run from the **repository root** (see [root README](../README.md) for full task list):

```bash
bb rct remote     # Run RCT tests only
bb test remote    # Run full Kaocha test suite (RCT + integration)
bb dev remote     # Start REPL
```
