---
name: clojure
description: Guide to plan/design/coding in Clojure. Use when working with Clojure projects, .clj files, deps.edn/project.clj configuration, or when the user asks about Clojure idioms, macros, or functional programming patterns. Covers REPL-driven development, data-oriented design, and core Clojure practices.
---

# Clojure

## Core Principles

### REPL-First Exploration
**DO NOT use grep, Glob, or text search to explore Clojure code.** Connect to a REPL and use namespace introspection. See "nREPL Workflow" section.

### Data-Oriented Design
- Prefer plain data (maps, vectors, sets) over custom types
- Use keywords as keys: `{:name "Alice" :age 30}`
- Data is immutable by default; embrace it

### Pure Functions First
- Write pure functions that transform data
- Isolate side effects to system edges
- Compose small functions into larger workflows

### Reuse Before Writing
Before creating a new function, search for existing code that does the same thing or something similar that could be generalized.

### Requirements as Examples
Most requirements are **examples of common scenarios** - design for the general case. Some are truly **one-off edge cases** - implement narrowly. If unsure, ask.

## Parallel Implementation with Subagents

Clojure's immutability and pure functions make parallel work **safe by design** - no shared mutable state means agents can't corrupt each other's work. Leverage this with jj workspaces for parallel feature implementation.

### When to Parallelize
During planning, identify **independent subtasks** that can be implemented concurrently:
- Functions with no shared dependencies
- Separate namespaces or modules
- Tests and implementation (write in parallel)
- Frontend and backend changes

### Workflow
1. **Plan phase**: Break task into independent features
2. **Spawn subagents**: Each in its own jj workspace
   ```bash
   jj workspace add feature-a /tmp/workspace-feature-a
   jj workspace add feature-b /tmp/workspace-feature-b
   ```
3. **Implement in parallel**: Each agent works in isolation
4. **Merge**: Bring workspaces back together
   ```bash
   jj workspace forget feature-a feature-b
   ```

### Planning Checklist
When planning a multi-part task, ask:
- [ ] Can this be split into 2-3 independent pieces?
- [ ] Do the pieces touch different files/namespaces?
- [ ] Is each piece well-defined enough to delegate?

If yes to all → spawn parallel subagents with jj workspaces.

## nREPL Workflow

Use **clj-nrepl-eval** for REPL-driven development.

### Setup
```bash
# Check for running REPLs
clj-nrepl-eval --discover-ports

# Start REPL if needed
clj -Sdeps '{:deps {nrepl/nrepl {:mvn/version "RELEASE"}}}' -M -m nrepl.cmdline

# Load introspection tools (required for doc, source, dir)
clj-nrepl-eval -p <port> "(require '[clojure.repl :refer [doc source dir]])"
```

### Explore Code
```bash
clj-nrepl-eval -p <port> "(map ns-name (all-ns))"     # List namespaces
clj-nrepl-eval -p <port> "(dir myapp.core)"           # List vars
clj-nrepl-eval -p <port> "(doc myapp.core/some-fn)"   # Function docs
clj-nrepl-eval -p <port> "(source myapp.core/some-fn)" # View source
```

### Understand Data
```bash
clj-nrepl-eval -p <port> "(keys some-map)"            # Map keys
clj-nrepl-eval -p <port> "(type result)"              # Check type
clj-nrepl-eval -p <port> "(first large-coll)"         # Sample data
```

### Development Cycle
1. Edit code in source files
2. Reload: `(require '[myapp.core :as core] :reload)`
3. Test: `(core/my-function arg)`
4. Iterate

### Delimiter Repair
Fix unbalanced parens: `clj-paren-repair <file.clj>`

## Idiomatic Patterns

### Threading Macros
```clojure
(-> person :address :city str/upper-case)            ; object-first
(->> orders (filter :paid?) (map :total) (reduce +)) ; collection-last
(some-> user :profile :avatar :url)                  ; nil-safe
```

### Destructuring
```clojure
(defn greet [{:keys [first-name last-name]}]
  (str "Hello, " first-name " " last-name))

(defn connect [{:keys [host port] :or {port 8080}}]
  (str host ":" port))

(let [[head & tail] items]
  (process head tail))
```

### Polymorphism
```clojure
;; Multimethods for open dispatch
(defmulti process-event :type)
(defmethod process-event :click [e] ...)
(defmethod process-event :default [e] ...)

;; Protocols for type-based dispatch
(defprotocol Persistable
  (save! [this])
  (load! [this id]))
```

## Cross-Platform (.cljc)

### Prefer .cljc Over .clj/.cljs
Use `.cljc` by default. Use reader conditionals for platform differences:
```clojure
#?(:clj  (java.util.UUID/randomUUID)
   :cljs (random-uuid))
```

### Macros in .cljc Files
**A namespace cannot use its own macros during initial load in ClojureScript.**

```clojure
;; BAD: macro used in same namespace
(defmacro register! [type spec] `(swap! registry* assoc ~type ~spec))
(register! :foo {...})  ; FAILS in ClojureScript!

;; GOOD: convert to function if only doing runtime ops
(defn register! [type spec] (swap! registry* assoc type spec))

;; OR: move calls to separate namespace that requires this one
```

**Self-referential macros pattern:**
```clojure
(ns mylib.util
  #?(:cljs (:require-macros [mylib.util])))  ; Required for CLJS
(defmacro my-macro [...] ...)
```

## API Design

### Minimal Public Surface
- Public API namespace (e.g., `mylib.core`) with stable guarantees
- Implementation in internal namespaces (e.g., `mylib.internal.*`)
- Users CAN access internals but those MAY change

### Backward-Compatible Extensions
Use multi-arity for optional arguments:
```clojure
(defmacro my-macro
  ([required-arg] `(my-macro ~required-arg {}))
  ([required-arg opts] ...))
```

### Options Pass-Through
Public wrappers forward options to internal implementations.

## Testing

### Unit Tests: RCT (inline)
Place `^:rct/test` block **immediately after** the function:
```clojure
(defn add [a b]
  (+ a b))

^:rct/test
(comment
  (add 5 3) ;=> 8
  (add 0 0) ;=> 0)
```

**Syntax:** `;=>` equality, `;=>>` predicate/matcho, `;throws=>>` exceptions

### Integration Tests: test/ namespace
```clojure
(ns myapp.integration-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :each (fn [f] (db/with-test-conn f)))

(deftest api-integration-test
  (testing "end-to-end flow"
    (is (= expected (myapp.api/handler request)))))
```

### Run Tests
```bash
bb test              # All components
bb test <component>  # Specific component
```

## Code Style

- **Naming:** kebab-case, predicates end with ?, side-effects end with !
- **Threading:** prefer -> ->> cond-> cond->>
- **Conditionals:** use `when-let`, `if-let`
- **Docstrings:** every public var has a concise docstring

## Namespace Dependencies

A namespace can only require:
1. Its **children** (e.g., `foo.bar` → `foo.bar.baz`)
2. **External libraries** (e.g., `clojure.walk`)
3. **Exception:** one `util` namespace with pure functions only

```clojure
;; ALLOWED
foo.bar   → foo.bar.baz    ; child
foo.bar   → clojure.walk   ; external

;; NOT ALLOWED
foo.bar.a → foo.bar.b      ; sibling (causes cycles)
```

## Error Handling

```clojure
(throw (ex-info "Invalid input" {:field :email :value x}))

(try
  (process data)
  (catch ExceptionInfo e
    (handle-error (:field (ex-data e)))))
```

## Post-Task Review

**After completing any non-trivial coding task, review for simplification:**

1. **Delete** - unused vars, dead branches, unnecessary nil checks
2. **Inline** - single-use helpers that obscure rather than clarify
3. **Flatten** - nested maps → flat maps, custom types → plain data
4. **Combine** - multiple passes → single pass, separate conditions → `cond`
5. **Remove abstractions** - don't generalize for hypothetical futures

Ask: "If I were reading this for the first time, what would confuse me?"

## Capture Findings

Add `NOTE:` comments for non-obvious behavior:
```clojure
;; NOTE: api-pull returns symbol keys, not keywords
(get response 'all)
```

## Self-Improvement

**Update this skill when you discover useful Clojure knowledge.**

Add new learnings for: gotchas, useful patterns, library idioms, performance tips, common mistakes.

**How:** Add to appropriate section, or to "Learned Patterns" below.

**Location:** `$CLAUDE_PROJECT_DIR/.claude/skills/clojure/SKILL.md`

## Learned Patterns

<!-- Add new discoveries below this line -->

## Subskills

- **clojurescript**: Use when working with `.cljs` files, `shadow-cljs.edn`, or frontend builds
