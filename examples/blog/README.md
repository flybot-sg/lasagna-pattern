# Pull Blog Example

A personal blog demonstrating the pull-based remote API with noun-only CRUD.

## Quick Start

```bash
# From this directory
clj -M:dev
```

Then in the REPL:

```clojure
(require '[sg.flybot.blog.system :refer [make-system]]
         '[sg.flybot.pullable.remote.client :as client]
         '[robertluo.fun-map :refer [halt!]])

;; Start system
(def sys (make-system {:port 8080}))
(:server sys)  ; starts server

;; Connect client
(def api (client/connect "http://localhost:8080/api"))

;; List all posts
(api '{:posts ?posts})

;; Get single post by id
(api '{:posts {{:id 1} ?post}})

;; Stop
(halt! sys)
```

## Noun-Only CRUD

The schema is purely nouns - no verbs like `create-post` or `update-post`.
CRUD operations are expressed through pattern syntax on collections:

| Pattern | Operation |
|---------|-----------|
| `{:posts ?all}` | LIST all posts |
| `{:posts {{:id 3} ?post}}` | READ post by id |
| `{:posts {nil {:title "..."}}}` | CREATE new post |
| `{:posts {{:id 3} {:title "..."}}}` | UPDATE post |
| `{:posts {{:id 3} nil}}` | DELETE post |

## Role-Based Authentication

Enable auth by passing `:owner-emails`:

```clojure
(def sys (make-system {:port 8080
                       :owner-emails #{"alice@example.com"}}))
```

| User | Schema |
|------|--------|
| Anonymous / non-owner | Read-only (list, get posts) |
| Owner (email in whitelist) | Full CRUD + `:me` endpoint |

The session must contain `:user-email` - integrate with your auth middleware
(Google SSO, Auth0, etc.) to set this.

## Files

| File | Description |
|------|-------------|
| `src/.../db.cljc` | In-memory database with DataSource |
| `src/.../api.cljc` | Schema and API builder |
| `src/.../auth.cljc` | Role-based schema selection |
| `src/.../system.clj` | System integration (life-cycle-map) |
| `src/.../server.clj` | Standalone server |

## Testing

```bash
clj -X:dev:test
```
