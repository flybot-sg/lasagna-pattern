# Flybot Site

Migration of flybot.sg company site. Internal portal with blog-based content for company updates, announcements, and knowledge sharing.

## Quick Start

### Option 1: Full-Stack (Server + SPA)

```bash
# Build the frontend
npm install
npm run release

# Start the server
clj -M:dev -e "(require '[sg.flybot.flybot-site.server :as s]) (s/start!)"
```

Open http://localhost:8080 in your browser.

### Option 2: REPL-Driven Development

```bash
clj -M:dev
```

Then in the REPL:

```clojure
(require '[sg.flybot.flybot-site.system :refer [make-system]]
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

### Option 3: Frontend Development (Hot Reload)

```bash
# Terminal 1: Start backend
clj -M:dev -e "(require '[sg.flybot.flybot-site.server :as s]) (s/start!)"

# Terminal 2: Start shadow-cljs watcher
npm run dev
```

Frontend hot reloads at http://localhost:3000, proxies API to backend.

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

### Backend (Clojure)

| File | Description |
|------|-------------|
| `src/.../db.clj` | Datahike database with DataSource |
| `src/.../api.cljc` | Schema and API builder |
| `src/.../auth.cljc` | Role-based schema selection |
| `src/.../system.clj` | System integration (life-cycle-map) |
| `src/.../server.clj` | HTTP server with static file serving |

### Frontend (ClojureScript)

| File | Description |
|------|-------------|
| `src/.../ui/core.cljs` | App entry point, API actions, rendering |
| `src/.../ui/api.cljs` | Transit-based API client |
| `src/.../ui/state.cljc` | State management (testable on JVM) |
| `src/.../ui/views.cljc` | Hiccup views (testable on JVM) |

## Testing

```bash
# Run all tests (backend + frontend state/views)
clj -X:dev:test

# Build frontend release
npm run release
```
