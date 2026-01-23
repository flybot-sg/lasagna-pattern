# Pull Pattern Toolbox

A monorepo for pull-based pattern matching and data transformation tools in Clojure/ClojureScript.

**Use `jj` instead of `git`** - This is a jj/git co-located repo.

**Use `/clojure` skill** when working on this project.

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
| `remote/` | Remote protocol (GraphQL-like) using pattern language | Planned |

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

**Source:** `pattern/src/sg/flybot/pullable/core.cljc`

**Public API:**
- `match-fn` - Create pattern-matching functions with variable bindings
- `rule` - Pattern → template transformation rules
- `compile-pattern` - Low-level pattern compilation
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

**Deep dive:** See `pattern/doc/ARCHITECTURE.md` for internal design, matcher constructors, and extension points.

## Testing

Uses Rich Comment Tests (RCT) - inline tests in source:

```clojure
^:rct/test
(comment
  (some-fn 1 2) ;=> expected-result
  )
```

Run `bb test` to execute all tests.
