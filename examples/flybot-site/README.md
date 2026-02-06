# Flybot Site

Public company blog with employee-authored content.

## Rationale

This is an **example application** demonstrating how the pull pattern toolbox can be used in real-world web development. It shows:

- **Pattern-based API** - Role-scoped CRUD using pull patterns
- **Lazy collections** - Datahike wrapped as `ILookup` collections
- **Ownership wrappers** - Members can only edit their own posts
- **Full-stack Clojure** - Same patterns work client and server side

## How Components Are Used

| Component | Usage in flybot-site |
|-----------|---------------------|
| [pattern](../../pattern) | Pattern matching for API queries and mutations |
| [collection](../../collection) | DataSource protocol for posts/users with ownership wrappers |
| [remote](../../remote) | HTTP API handler that processes pull patterns from the frontend |

## Quick Start

All commands are run from the **repository root** (see [root README](../../README.md) for full task list):

```bash
bb dev examples/flybot-site    # Start nREPL (includes shadow-cljs support)
```

In REPL:
```clojure
(user/start!)  ; Start server on port 8080 (dev mode with auto-login)

;; For frontend hot reload:
(require '[shadow.cljs.devtools.server :as shadow-server])
(require '[shadow.cljs.devtools.api :as shadow])
(shadow-server/start!)
(shadow/watch :app)  ; Frontend at http://localhost:3000
```

## System Lifecycle

The system uses [fun-map](https://github.com/robertluo/fun-map) for dependency injection. See `dev/user.clj` for convenient functions to start/stop the system with different configurations:

```clojure
;; dev/user.clj provides:
(start!)                    ; Default dev mode (auto-login, in-memory DB)
(start! {:mode :dev-oauth}) ; Dev mode with real OAuth2 flow
(sys/stop!)                 ; Stop the system
```

The `dev-cfg` map in `user.clj` shows all available configuration options.

## Configuration

Environment variables (see `.env.example`):

| Variable | Description |
|----------|-------------|
| `SERVER_PORT` | HTTP port (default: 8080) |
| `DATAHIKE_BACKEND` | `:mem`, `:file`, or `:s3` |
| `GOOGLE_CLIENT_ID` | OAuth client ID |
| `BLOG_OWNER_EMAILS` | Comma-separated owner emails |

## Role-Based API

Patterns use role as top-level key:

```clojure
'{:guest {:posts ?all}}                              ; List posts (public)
'{:guest {:posts {{:post/id 1} ?post}}}              ; Read post (public)
{:member {:posts {nil {:post/title "New"}}}}         ; Create (member)
{:member {:posts {{:post/id 1} {:post/title "X"}}}}  ; Update own (member)
{:admin {:posts {{:post/id 1} nil}}}                 ; Delete any (admin)
```

## Architecture

```
src/sg/flybot/flybot_site/
├── server/
│   ├── system.clj       # fun-map DI system
│   └── system/
│       ├── api.clj      # API handler (uses remote + pattern)
│       ├── auth.clj     # OAuth + roles
│       ├── db.clj       # Datahike collections (uses collection)
│       └── cfg.cljc     # Config schema (Malli)
└── ui/
    ├── core.cljs        # Entry point + dispatch
    ├── state.cljc       # Pure state functions
    └── views.cljc       # Replicant hiccup
```

## Development

```bash
bb rct examples/flybot-site    # Run RCT tests only
bb test examples/flybot-site   # Run full Kaocha suite (RCT + integration tests)
```

This component has both RCT tests in source files and integration tests in `test/`.