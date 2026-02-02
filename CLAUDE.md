# Pull Pattern Toolbox

A monorepo for pull-based pattern matching and data transformation tools in Clojure/ClojureScript.

## Claude Code Plugins (Optional)

A local marketplace is available at `claude-ctx/` with enforced workflows:

```bash
/plugin marketplace add ./claude-ctx
```

Then enable in `.claude/settings.local.json`:
```json
{
  "enabledPlugins": {
    "lasagna-clj@lasagna": true,
    "lasagna-jj@lasagna": true
  }
}
```

| Plugin | What it does |
|--------|--------------|
| `lasagna-clj` | Enforces `/clojure` skill before editing .clj files, paren repair |
| `lasagna-jj` | Blocks git commands (jj only), test reminders before commits |

## Build System

All builds run from root via Babashka:

```bash
bb list              # List all components
bb test              # Test all components
bb test pattern      # Test specific component
bb dev pattern       # Start dev REPL for component
bb nrepl pattern     # Start nREPL for component
bb clean             # Clean all
bb clean pattern     # Clean specific component
```

Components are auto-discovered (any directory with `deps.edn`).

## Components

| Directory | Description | Status |
|-----------|-------------|--------|
| `pattern/` | Core pattern DSL for matching/transforming Clojure data | Active |
| `collection/` | CRUD collection abstraction with DataSource protocol | Active |
| `remote/` | Remote protocol (GraphQL-like) using pattern language | Active |
| `examples/flybot-site/` | Flybot.sg site migration - internal company portal | Active |

### Adding a New Component

1. Create directory with `deps.edn`:
   ```clojure
   {:paths ["src"]
    :deps {org.clojure/clojure {:mvn/version "1.12.4"}}
    :aliases
    {:dev {:extra-paths ["notebook"]
           :extra-deps {lambdaisland/kaocha {:mvn/version "1.91.1392"}
                        io.github.robertluo/rich-comment-tests {:mvn/version "1.1.78"}}}
     :test {:exec-fn com.mjdowney.rich-comment-tests.test-runner/run-tests-in-file-tree!
            :exec-args {:dirs #{"src"}}}}}
   ```

2. For local dependencies on other components:
   ```clojure
   {:deps {local/pattern {:local/root "../pattern"}}}
   ```

3. Component is auto-discovered by `bb list`.

## Component: pattern

Core pattern DSL enabling declarative matching and transformation of Clojure data structures.

**Source:** `pattern/src/sg/flybot/pullable/impl.cljc`

**Public API:**
- `match-fn` - Create pattern-matching functions with variable bindings (supports `:schema`, `:rules` options)
- `rule` - Pattern → template transformation rules
- `apply-rules` - Recursive tree transformation

**Pattern syntax:**
```clojure
?x       ; Bind value to x
?_       ; Wildcard (match anything)
?x?      ; Optional (0-1)
?x*      ; Zero or more
?x+      ; One or more
{}       ; Map pattern
[]       ; Sequence pattern
(?x :when pred)    ; Constrained match
(?x :default val)  ; Default on failure
```

**Map matching:**
- Supports maps and any `ILookup` implementation (lazy data sources)
- Maps preserve unmatched keys (passthrough semantics)
- ILookup returns only matched keys (can't enumerate all keys)
- Non-keyword keys for indexed lookup: `{{:id 1} ?result}`
- With Malli schemas: indexed lookup requires `:ilookup true` on collection (e.g., `[:vector {:ilookup true} ...]`)

**Deep dive:** See `pattern/doc/ARCHITECTURE.md` for internal design, matcher constructors, and extension points.

## Flybot Site (examples/flybot-site)

Migration of flybot.sg company site. Internal portal with blog-based content for company updates, announcements, and knowledge sharing. Demonstrates noun-only API design with pattern-based CRUD.

**Stack:** Clojure + ClojureScript, Datahike, http-kit, Replicant SPA, Google OAuth

**Source:** `examples/flybot-site/src/sg/flybot/flybot_site/`

**Run:**
```bash
cd examples/flybot-site
clj -M:dev:cljs  # Compile ClojureScript
clj -M:run       # Start server at localhost:8080
```

**API design (noun-only):**
```clojure
;; Schema defines nouns, not verbs
{:posts [:union [post-schema] {post-query post-schema}]}

;; CRUD via pattern syntax:
{:posts ?all}                    ; LIST
{:posts {{:post/id 3} ?post}}    ; READ (indexed lookup)
{:posts {nil {:post/title ...}}} ; CREATE
{:posts {{:post/id 3} {...}}}    ; UPDATE
{:posts {{:post/id 3} nil}}      ; DELETE
```

**Key patterns demonstrated:**
- ILookup-based collections for lazy data access
- Schema-driven output filtering
- Frontmatter extraction from markdown content

### Portal Logging

The portal has a unified logging system for both frontend and backend.

**Backend:** `examples/flybot-site/src/sg/flybot/flybot_site/log.clj`
- Uses Timbre (included via Datahike)
- Levels: `:trace` `:debug` `:info` `:warn` `:error` `:fatal`

```clojure
(require '[sg.flybot.flybot-site.log :as log])

;; Basic logging
(log/debug "value:" x)
(log/info "Server started")
(log/error "Failed:" err)

;; Set level (e.g., for production)
(log/set-level! :info)

;; Domain-specific (prefer these for consistency)
(log/log-api-request pattern)
(log/log-api-response response)
(log/log-api-error err pattern)
(log/log-db-op :fetch :post id)
(log/log-db-create :post entity)
(log/log-db-update :post id)
(log/log-db-delete :post id)
(log/log-startup port)
(log/log-shutdown)

;; Ring middleware for request timing
(-> handler (log/wrap-request-logging))
```

**Frontend:** `examples/flybot-site/src/sg/flybot/flybot_site/ui/log.cljc`
- Cross-platform (.cljc) for JVM testability
- Browser: uses `console.debug/info/warn/error`
- JVM: uses `println`
- Levels: `:debug` `:info` `:warn` `:error` `:off`

```clojure
(require '[sg.flybot.flybot-site.ui.log :as log])

;; Basic logging
(log/debug "value:" x)
(log/info "User action")
(log/warn "Deprecation")
(log/error "Failed:" err)

;; Configure
(log/set-level! :warn)  ; Only warn and error
(log/set-prefix! "[myapp]")

;; Domain-specific
(log/log-api-request pattern)
(log/log-api-response response)
(log/log-api-error err pattern)
(log/log-state-change "action" old-state new-state)
```

**Integration points:**
- Backend: `server.clj`, `db.clj`, `system.clj`
- Frontend: `api.cljs`, `core.cljs`

## Testing

Uses Rich Comment Tests (RCT). Run `bb test` to execute all tests.

```clojure
^:rct/test
(comment
  (some-fn 1 2) ;=> expected-result
  )
```
