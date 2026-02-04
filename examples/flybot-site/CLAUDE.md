# Flybot Site

Migration of [flybot.sg](https://flybot.sg) company site. Public company blog with employee-authored content.

## Purpose

This is the new flybot.sg site built with the pull-pattern toolbox:

- **Public access**: Anyone can read posts (no login required)
- **Employee authoring**: Only employees can log in and create posts
- **Authentication**: Google OAuth with email pattern restriction

## Access Model

Roles are stored as a set in the session and DB (e.g., `#{:member :admin}`).

| Role | Who | Permissions |
|------|-----|-------------|
| `:guest` | No session | Read all posts |
| `:member` | Logged-in employees | Read + Create/Edit/Delete **own** posts |
| `:admin` | Granted by owner | Read + Create/Edit/Delete **any** post |
| `:owner` | `BLOG_OWNER_EMAILS` | All above + grant/revoke admin role |

**Role Storage:**
- Roles stored in DB with grant timestamps
- On first login: auto-grants `:member` (or all 3 for owner emails)
- Roles cached in session cookie (no DB lookup per request)
- User gets new roles on **next login** after grant/revoke

**Configuration:**
- `BLOG_ALLOWED_EMAILS` - Regex for who can log in (e.g., `.*@company\.com$`)
- `BLOG_OWNER_EMAILS` - Comma-separated admin emails (get all roles on first login)

**API Structure (role as top-level keys):**
```clojure
{:guest  {:posts read-only}
 :member {:posts member-crud, :posts/history lookup, :me user-info}
 :admin  {:posts admin-crud}
 :owner  {:users users-collection}}
```

Role keys are nil if session lacks the required role. Patterns:
```clojure
'{:guest {:posts ?all}}                           ; guest list
{:member {:posts {nil {:post/title ...}}}}        ; member create
{:member {:posts {{:post/id 1} nil}}}             ; member delete own
'{:member {:posts/history {{:post/id 1} ?v}}}     ; member history
{:admin {:posts {{:post/id 1} {...}}}}            ; admin update any
'{:owner {:users ?all}}                           ; owner list users
```

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

Role is always the top-level key. Each role contains the resources it can access.

```clojure
;; List all posts (guest/public)
'{:guest {:posts ?all}}

;; Get single post by ID (guest/public)
'{:guest {:posts {{:post/id 3} ?post}}}

;; Create (member - nil key = new)
{:member {:posts {nil {:post/title "New" :post/content "..."}}}}

;; Update own post (member)
{:member {:posts {{:post/id 3} {:post/title "Updated"}}}}

;; Delete own post (member - nil value = delete)
{:member {:posts {{:post/id 3} nil}}}

;; Update any post (admin)
{:admin {:posts {{:post/id 3} {:post/title "Admin Edit"}}}}

;; Get current user info (member)
'{:member {:me ?user}}

;; List users (owner)
'{:owner {:users ?all}}

;; History (public via guest)
'{:guest {:posts/history {{:post/id 3} ?versions}}}
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
