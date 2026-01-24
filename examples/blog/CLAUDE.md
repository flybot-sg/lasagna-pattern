# Blog Example

Full-stack blog demonstrating pull-based pattern matching with Clojure/ClojureScript.

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
src/sg/flybot/blog/
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

**File:** `src/sg/flybot/blog/log.clj`

```clojure
(require '[sg.flybot.blog.log :as log])

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
2024-01-24T19:13:06.710Z INFO [sg.flybot.blog.log:86] - DB created :post id= 1
```

### Frontend Logging

**File:** `src/sg/flybot/blog/ui/log.cljc`

Cross-platform (.cljc) so logic can be tested on JVM.

```clojure
(require '[sg.flybot.blog.ui.log :as log])

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
[blog] [debug] "API request:" {:posts ?all}
[blog] [info] "Image uploaded:" "/uploads/abc.png"
[blog] [error] "API error for" {:posts ?all} ":" "HTTP 500"
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
(require '[sg.flybot.blog.db :as db])

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
