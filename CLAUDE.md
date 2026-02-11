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
| `examples/flybot-site/` | Flybot.sg site - public blog, employee authoring | Active |
| `examples/pull-playground/` | Interactive SPA for learning pull patterns (sandbox + remote) | Active |

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

**Utilities:** `sg.flybot.pullable.util` — `variable?` (check if symbol is `?`-prefixed), `contains-variables?` (recursive tree-walk for nested patterns), `vars->` (macro for destructuring vars maps).

**Deep dive:** See `pattern/CLAUDE.md` for full syntax catalog, architecture, and extension points.

## Flybot Site (examples/flybot-site)

Public company blog with employee-authored content. Demonstrates role-based API with pattern CRUD.

**Access model (role-as-top-level):**

| Role | Who | Permissions |
|------|-----|-------------|
| `:guest` | Anonymous | Read posts |
| `:member` | Logged-in | Read + CRUD own posts + view history |
| `:admin` | Granted | CRUD any post |
| `:owner` | Config | All above + manage users/roles |

**Stack:** Clojure + ClojureScript, Datahike, http-kit, Replicant SPA, Google OAuth

**Source:** `examples/flybot-site/src/sg/flybot/flybot_site/server/`

**Run:**
```bash
bb dev examples/flybot-site  # Start nREPL
# Then in REPL: (user/start!)
```

**API design (role-as-top-level):**
```clojure
;; Each role key contains its accessible resources
'{:guest {:posts ?all}}                           ; guest list
'{:guest {:posts {{:post/id 1} ?post}}}           ; guest read
{:member {:posts {nil {:post/title "New"}}}}      ; member create
{:member {:posts {{:post/id 1} {:post/title "X"}}}} ; member update own
{:member {:posts {{:post/id 1} nil}}}             ; member delete own
'{:member {:posts/history {{:post/id 1} ?v}}}     ; member history
{:admin {:posts {{:post/id 1} {:post/title "X"}}}} ; admin update any
'{:owner {:users ?all}}                           ; owner list users
```

**Key patterns demonstrated:**
- Role-as-top-level authorization (nil if session lacks role)
- ILookup-based collections for lazy data access
- Ownership enforcement via collection wrappers

## Testing

Uses Rich Comment Tests (RCT). Run `bb test` to execute all tests.

```clojure
^:rct/test
(comment
  (some-fn 1 2) ;=> expected-result
  )
```
