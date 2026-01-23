# Pull Blog Example

A personal blog demonstrating the pull-based remote API over HTTP.

## Quick Start

```bash
# From repo root
bb dev examples/blog

# Or from this directory
clojure -M:dev
```

Then in the REPL:

```clojure
;; Start system (creates db, app, server as one unit)
(start!)
;; Blog server started on port 8080

;; Connect client
(def api (connect))

;; Pull data
(api '{:posts ?posts})

;; Single post
(api '{:post {:title ?t}} {:post-id 1})

;; Stop system (halts server, clears db)
(stop!)
```

## Architecture

```
┌─────────┐  HTTP/Transit  ┌─────────────┐  Pattern   ┌──────────┐
│ Client  │ ──────────────→│ Ring Handler│ ─────────→ │ Lazy API │
│ (REPL)  │                │ (http-kit)  │            │ (fun-map)│
└─────────┘                └─────────────┘            └──────────┘
```

### System Integration

The system uses `life-cycle-map` from fun-map to manage components:

```clojure
(life-cycle-map
  {:port port
   :api-fn (fnk [] (make-api-fn db))     ; depends on db
   :app    (fnk [api-fn] (make-app ...)) ; depends on api-fn
   :server (fnk [app port] ...)})        ; depends on app, port
```

- Components start lazily when accessed
- `halt!` stops everything in reverse order
- Dependencies are tracked automatically

## Client API

```clojure
(require '[sg.flybot.pullable.remote.client :as client])

;; Connect to server
(def api (client/connect "http://localhost:8080/api"))

;; Pull data (returns {:data ... :vars ...})
(api '{:posts ?posts})
(api '{:post {:title ?t}} {:post-id 1})

;; Get schema
(client/schema api)
```

## Pattern Examples

### Query: List Posts

```clojure
(api '{:posts ?posts})
;; => {:data {...} :vars {'posts [{:id 1 :title "Hello" ...} ...]}}
```

### Query: Single Post

```clojure
(api '{:post {:id ?id :title ?t :content ?c}}
     {:post-id 1})
```

### Mutation: Create Post

```clojure
(api '{:create-post {:id ?id :title ?t}}
     {:create-post {:title "New" :content "..." :author "Me"}})
```

## Files

| File | Description |
|------|-------------|
| `src/sg/flybot/blog/db.cljc` | In-memory database |
| `src/sg/flybot/blog/api.cljc` | Lazy API definition with schema |
| `src/sg/flybot/blog/system.clj` | System integration (life-cycle-map) |
| `src/sg/flybot/blog/server.clj` | Standalone server (no system) |
| `dev/user.clj` | REPL helpers and demo session |

## Testing

```bash
# From repo root
bb test examples/blog

# Or directly
clojure -X:dev:test
```
