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
# Backend
clj -M:run              # Start server at localhost:8080

# Frontend (separate terminal)
npm install
npm run dev             # shadow-cljs watch with hot reload
```

## Architecture

```
src/sg/flybot/flybot_site/
├── api.cljc            # API schema and data structure
├── auth.cljc           # Role-based authentication
├── db.clj              # Datahike database + DataSource impl
├── log.clj             # Backend logging (Timbre)
├── markdown.cljc       # Frontmatter parsing
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

Use logging for debugging API calls, database operations, and state changes.

### Backend Logging

**File:** `src/sg/flybot/flybot_site/log.clj`

```clojure
(require '[sg.flybot.flybot-site.log :as log])

;; Basic
(log/debug "Processing:" data)
(log/info "User logged in:" email)
(log/warn "Deprecated API called")
(log/error "Database error:" ex)

;; Set level
(log/set-level! :info)  ; :trace :debug :info :warn :error :fatal

;; Domain-specific helpers
(log/log-api-request '{:posts ?all})
(log/log-api-response {:posts [...]})
(log/log-api-error ex '{:posts ?all})

(log/log-db-op :fetch :post 123)
(log/log-db-create :post {:post/id 1 :post/title "New"})
(log/log-db-update :post 123)
(log/log-db-delete :post 123)

(log/log-startup 8080)
(log/log-shutdown)
(log/log-db-connected {:backend :mem :id "blog"})
(log/log-db-seeded 3)

;; Ring middleware (adds request timing)
(-> handler (log/wrap-request-logging))
```

**Output format:**
```
2024-01-24T19:13:06.710Z INFO [sg.flybot.flybot-site.log:86] - DB created :post id= 1
```

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

| Event | Level | Logger |
|-------|-------|--------|
| API request/response | debug | `log-api-request/response` |
| API error | error | `log-api-error` |
| DB read operation | debug | `log-db-op` |
| DB write operation | info | `log-db-create/update/delete` |
| Server start/stop | info | `log-startup/shutdown` |
| State view change | debug | `log-state-change` |
| Image upload success | info | `log/info` |
| Image upload failure | error | `log/error` |

### Production Configuration

```clojure
;; Backend - set in system startup
(log/set-level! :info)

;; Frontend - set in init!
(log/set-level! :warn)
```

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
