# Flybot Site

Migration of [flybot.sg](https://flybot.sg) company site. Internal portal with blog-based content for company updates, announcements, and knowledge sharing.

## Purpose

This is the new flybot.sg site built with the pull-pattern toolbox:

- **Audience**: Company employees only
- **Content**: Internal announcements, technical posts, team updates
- **Authentication**: Google OAuth restricted to company domain
- **Roles**: Admin (full access), Author (create/edit own posts), Reader (view only)

## Tech Stack

Full-stack Clojure/ClojureScript demonstrating pull-based pattern matching.

## Quick Start

```bash
# From repo root - starts nREPL with both CLJ and CLJS support
bb dev examples/flybot-site
```

### REPL Development

Go to `dev/user.clj` namespace — it has everything you need:

```clojure
(start!)       ; Start server (config from .env)
(sys/stop!)    ; Stop server
(sys/restart!) ; Restart server
```

### nREPL Connection (for Claude Code)

After user runs `bb dev examples/flybot-site`, connect to the nREPL:

```bash
# Port is in .nrepl-port file (user will provide port number)
clj-nrepl-eval -p <port> "(require 'user)"
clj-nrepl-eval -p <port> "(user/start!)"
```

**Important:**
- Use `:reload`, never `:reload-all` (breaks core.async protocols)
- If REPL state is corrupted, ask user to restart with `bb dev`
- Backend serves on 8080, shadow-cljs hot reload on 3000

### Restarting After Code Changes (fun-map pattern)

When you modify backend files (e.g., db.clj, system.clj), follow these steps:

```bash
# 1. Stop the system
clj-nrepl-eval -p <port> "(sg.flybot.flybot-site.system/stop!)"

# 2. Reload the changed namespace
clj-nrepl-eval -p <port> "(require 'sg.flybot.flybot-site.db :reload)"

# 3. Start the system again
clj-nrepl-eval -p <port> "(user/start!)"
```

**Note:** Frontend files (.cljs, .cljc used by frontend) auto-reload via shadow-cljs on save.

### Ports

| Port | Purpose |
|------|---------|
| 8080 | Backend server (prepackaged JS) |
| 3000 | shadow-cljs dev server (hot reload) |

## Architecture

```
src/sg/flybot/flybot_site/
├── api.cljc            # API schema and data structure
├── auth.cljc           # Role-based authentication
├── db.clj              # Datahike database + DataSource impl
├── markdown.cljc       # Frontmatter parsing
├── s3.clj              # S3 upload handler
├── server.clj          # HTTP server (standalone)
├── system.clj          # System lifecycle (fun-map)
└── ui/
    ├── api.cljs        # Frontend API client (fetch)
    ├── core.cljs       # App entry point, state, actions
    ├── log.cljc        # Frontend logging (console)
    ├── state.cljc      # Pure state transitions
    └── views.cljc      # Replicant hiccup views
```

## Logging

Use mulog for structured logging. Events are keyword-namespaced with key-value pairs.

### Backend Logging

**Library:** [mulog](https://github.com/BrunoBonacci/mulog)

```clojure
(require '[com.brunobonacci.mulog :as mu])

;; Structured event logging
(mu/log ::server-started :port 8080)
(mu/log ::db-create :entity :post :id 1)
(mu/log ::api-request :pattern '{:posts ?all})
(mu/log ::upload-complete :url "/uploads/abc.png")

;; Error logging
(mu/log ::db-error :error (ex-message e) :query query)
```

**Output:** JSON or console depending on publisher config in `system.clj`.

### Frontend Logging

**File:** `src/sg/flybot/flybot_site/ui/log.cljc`

Cross-platform (.cljc) so logic can be tested on JVM.

```clojure
(require '[sg.flybot.flybot-site.ui.log :as log])

;; Basic
(log/debug "Component mounted:" id)
(log/info "Form submitted")
(log/warn "Large payload:" (count items))
(log/error "Fetch failed:" err)

;; Configure
(log/set-level! :warn)   ; :debug :info :warn :error :off
(log/set-prefix! "[app]")

;; Domain-specific helpers
(log/log-api-request '{:posts ?all})
(log/log-api-response {:posts [...]})
(log/log-api-error err '{:posts ?all})
(log/log-state-change "set-view" old-state new-state)
```

**Browser output:**
```
[flybot-site] [debug] "API request:" {:posts ?all}
[flybot-site] [info] "Image uploaded:" "/uploads/abc.png"
[flybot-site] [error] "API error for" {:posts ?all} ":" "HTTP 500"
```

### When to Log

| Event | Example |
|-------|---------|
| DB operations | `(mu/log ::db-create :entity :post :id 1)` |
| API requests | `(mu/log ::api-request :pattern pattern)` |
| Errors | `(mu/log ::db-error :error (ex-message e))` |
| Lifecycle | `(mu/log ::server-started :port 8080)` |
| Uploads | `(mu/log ::upload-complete :url url)` |

## Database

Uses Datahike with in-memory store by default.

```clojure
(require '[sg.flybot.flybot-site.db :as db])

;; Create connection
(def conn (db/create-conn!))

;; Get posts collection (implements ILookup + Seqable)
(def p (db/posts conn))

;; CRUD
(seq p)                              ; list all
(get p {:post/id 3})                 ; fetch by id
(coll/mutate! p nil {:post/title "New"})      ; create
(coll/mutate! p {:post/id 3} {:post/title "Updated"}) ; update
(coll/mutate! p {:post/id 3} nil)    ; delete

;; History
(db/post-history @conn 3)            ; all versions of post 3
```

## API Patterns

```clojure
;; List all posts
'{:posts ?all}

;; Get single post by ID
'{:posts {{:post/id 3} ?post}}

;; Create (nil key = new)
{:posts {nil {:post/title "New" :post/content "..."}}}

;; Update
{:posts {{:post/id 3} {:post/title "Updated"}}}

;; Delete (nil value = delete)
{:posts {{:post/id 3} nil}}

;; History
'{:posts/history {{:post/id 3} ?versions}}
```

## Pages

Pages are special tag filters with different styling. A "page" is just a tag in the `:pages` set.

### Configuration

In `ui/state.cljc`:

```clojure
:pages #{"Home"}   ; tags that become pages
```

### Behavior

- **Navigation**: Each page gets a nav tab; clicking sets `:tag-filter` to that tag
- **Exclusion**: Posts with ONLY page tags are hidden from "Posts" view
- **Mixed tags**: Posts with page + regular tags appear in both views
- **URLs**: Pages use `/page/Home`, regular list uses `/`
- **Styling**: Page mode shows minimal cards (no author/date/tags, no height limit)

### Implementation

Pages reuse the existing tag filter mechanism:
- `page-mode?` selector checks if current `:tag-filter` is in `:pages` set
- `post-list-view` conditionally renders page styling when `page-mode?` is true
- No separate `:page` view type needed

## Testing

Uses Rich Comment Tests (RCT):

```bash
clj -X:dev:test
```

Tests are inline in source files:

```clojure
^:rct/test
(comment
  (some-fn 1 2) ;=> expected
  )
```
