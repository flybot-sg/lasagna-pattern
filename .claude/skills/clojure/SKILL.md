---
name: clojure
description: Guide to plan/design/coding in Clojure. Use when working with Clojure projects, .clj files, deps.edn/project.clj configuration, or when the user asks about Clojure idioms, macros, or functional programming patterns. Covers REPL-driven development, data-oriented design, and core Clojure practices.
---

# Clojure

## Design Principles

### REPL-First, Not Grep
**DO NOT use grep, Glob, or text search to explore Clojure code.** Always connect to a REPL first and use namespace introspection (`all-ns`, `dir`, `doc`, `source`). See "nREPL Workflow" section for commands.

### Requirements as Examples
Judge whether a requirement describes a specific scenario or a broader pattern:
- Most requirements are **examples of common scenarios** - design for the general case
- Some are truly **one-off edge cases** - implement narrowly
- If unsure, ask: "Is this a specific case, or should I design for similar scenarios?"

### Prefer .cljc Over .clj/.cljs
Use `.cljc` by default for code shareable across Clojure and ClojureScript. Use reader conditionals for platform differences:
```clojure
#?(:clj  (java.util.UUID/randomUUID)
   :cljs (random-uuid))
```

### Macros in .cljc Files

**Critical: A namespace cannot use its own macros during initial load in ClojureScript.**

When a `.cljc` file defines a macro and tries to call it at the top level of the same file, ClojureScript will fail with "Can't take value of macro" warnings and runtime errors.

```clojure
;; BAD: macro defined and used in same namespace
(ns mylib.impl)

(defmacro register! [type spec]
  `(swap! registry* assoc ~type ~spec))

(register! :foo {...})  ; FAILS in ClojureScript!
```

**Solutions:**

1. **Convert to function if possible** - If the macro only does runtime operations (like `swap!`), use a function instead:
```clojure
;; GOOD: function works in both Clojure and ClojureScript
(defn register! [type spec]
  (swap! registry* assoc type spec))

(register! :foo {...})  ; Works!
```

2. **Move calls to separate namespace** - If macro semantics are needed:
```clojure
;; mylib/impl.cljc - defines macro
(ns mylib.impl
  #?(:cljs (:require-macros [mylib.impl])))

(defmacro register! [type spec] ...)

;; mylib/registry.cljc - uses macro (requires impl)
(ns mylib.registry
  (:require [mylib.impl :refer [register!]]))

(register! :foo {...})  ; Works!
```

3. **Use `#?(:clj ...)` for JVM-only code** - Skip in ClojureScript if not needed:
```clojure
#?(:clj
   (register! :foo {...}))  ; Only runs on JVM
```

**Self-referential macros pattern:**
```clojure
(ns mylib.util
  #?(:cljs (:require-macros [mylib.util])))  ; Required for CLJS

(defmacro my-macro [...] ...)
```

### Data-Oriented Design
- Prefer plain data (maps, vectors, sets) over custom types
- Use keywords as keys: `{:name "Alice" :age 30}`
- Data is immutable by default; embrace it

### Pure Functions First
- Write pure functions that transform data
- Isolate side effects to system edges
- Compose small functions into larger workflows

### Reuse Before Writing
Before creating a new function, search for existing code that:
- Does the same thing
- Does something similar that could be generalized
- Lives in a parent namespace or util

## nREPL Workflow

Use **clj-nrepl-eval** for REPL-driven development.

### CRITICAL: REPL-First Exploration

**DO NOT use grep, Glob, or file search to explore Clojure code.** Instead, use REPL introspection.

**First step for ANY Clojure task:** Check for a running REPL or start one:

```bash
# Check for running REPLs
clj-nrepl-eval --discover-ports

# If no REPL, start one (in project directory)
clj -Sdeps '{:deps {nrepl/nrepl {:mvn/version "RELEASE"}}}' -M -m nrepl.cmdline
```

**Then explore via REPL, not grep:**

```bash
# List all namespaces to understand project structure
clj-nrepl-eval -p <port> "(map ns-name (all-ns))"

# Check namespace docstring/metadata for purpose
clj-nrepl-eval -p <port> "(-> 'myapp.core find-ns meta)"

# List all public vars in a namespace
clj-nrepl-eval -p <port> "(dir myapp.core)"

# Get function documentation
clj-nrepl-eval -p <port> "(doc myapp.core/some-fn)"

# View source code
clj-nrepl-eval -p <port> "(source myapp.core/some-fn)"
```

This is how Clojure developers work. The REPL provides accurate, live information about loaded code.

**Discover the environment:**
```bash
clj-nrepl-eval --discover-ports              # Find running REPLs
clj-nrepl-eval -p <port> "(all-ns)"          # List namespaces
clj-nrepl-eval -p <port> "(dir myapp.core)"  # List vars in namespace
clj-nrepl-eval -p <port> "(doc myapp.core/process-data)"  # Function docs
clj-nrepl-eval -p <port> "(source myapp.core/process-data)"  # View source
```

**Understand data shapes:**
```bash
clj-nrepl-eval -p <port> "(keys some-map)"           # Map keys
clj-nrepl-eval -p <port> "(type result)"             # Check type
clj-nrepl-eval -p <port> "(first large-collection)"  # Sample data
clj-nrepl-eval -p <port> "(meta #'some-var)"         # Var metadata
```

**Test assumptions interactively:**
```bash
clj-nrepl-eval -p <port> "(some-fn test-input)"      # Try function
clj-nrepl-eval -p <port> "(-> data transform1 transform2)"  # Build pipelines
```

**When to explore:**
- Before writing any new function—understand inputs and expected outputs
- When adding a new dependency—try the library's API interactively before integrating
- When understanding existing code—eval functions, inspect data flow, trace execution
- When debugging—inspect actual values, not assumed ones
- When refactoring—verify behavior before and after changes

### Development Cycle
1. Start REPL:
   ```bash
   clj -Sdeps '{:deps {nrepl/nrepl {:mvn/version "RELEASE"}}}' -M -m nrepl.cmdline
   ```
2. Edit code in source files
3. Reload and test:
   ```bash
   clj-nrepl-eval -p <port> "(require '[myapp.core :as core] :reload)"
   clj-nrepl-eval -p <port> "(core/my-function arg)"
   ```
4. Iterate: edit, reload, eval, refine

### Delimiter Repair
Fix unbalanced parens with [clojure-mcp-light](https://github.com/bhauman/clojure-mcp-light):
```bash
clj-paren-repair <file.clj>
```

## Idiomatic Patterns

### Threading Macros
```clojure
(-> person :address :city str/upper-case)           ; object-first
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

## API Design

### Minimal Public Surface
Expose the fewest functions necessary. More public functions = more promises to keep:
- Create a public API namespace (e.g., `mylib.core`) with stable guarantees
- Keep implementation in internal namespaces (e.g., `mylib.internal.*`)
- Users CAN access internals but those MAY change between versions

```clojure
;; mylib.core - public API, stable
(ns mylib.core
  (:require [mylib.internal.impl :as impl]))

(def my-fn impl/my-fn)  ; expose only what users need
```

### Backward-Compatible Extensions
When adding optional arguments to macros/functions, use multi-arity to avoid breaking existing callers:

```clojure
(defmacro my-macro
  ([required-arg] `(my-macro ~required-arg {}))
  ([required-arg opts]
   ;; implementation uses opts
   ...))

;; Existing code still works:
(my-macro pattern)
;; New code can use options:
(my-macro pattern {:schema user-schema})
```

### Options Pass-Through
Public wrappers should forward options to internal implementations, so users get features without calling internals:

```clojure
;; Public API - passes options through to internal
(defmacro match-fn
  ([pattern body] `(internal/match-fn ~pattern ~body))
  ([pattern body opts] `(internal/match-fn ~pattern ~body ~opts)))
```

## Code Smells

### Exhaustive Set Checks
Avoid checking membership against fixed sets - breaks when options expand:

```clojure
;; Bad: brittle
(when (#{:option1 :option2} option) ...)

;; Better: check exclusions or use multimethods
(when-not (#{:deprecated :disabled} option) ...)
```

## Project Structure

### deps.edn
```clojure
{:paths ["src" "resources"]
 :deps {org.clojure/clojure {:mvn/version "1.11.1"}
        io.github.robertluo/rich-comment-tests {:mvn/version "1.1.2"}}
 :aliases
 {:dev {:extra-paths ["dev"]}
  :test {:extra-deps {lambdaisland/kaocha {:mvn/version "1.91.1392"}}}}}
```

### Running Tests
```bash
clj -M:test -m kaocha.runner
```

### Namespace Conventions
```clojure
(ns myapp.core
  (:require
   [clojure.string :as str]
   [myapp.db :as db])
  (:import
   [java.time Instant]))
```

## Error Handling

```clojure
(throw (ex-info "Invalid input" {:field :email :value x}))

(try
  (process data)
  (catch ExceptionInfo e
    (handle-error (:field (ex-data e)))))
```

## Testing

### Unit Tests: RCT (inline in source)

Use rich-comment-tests for unit tests. Place `^:rct/test` block **immediately after** the function it tests:

```clojure
(defn add [a b]
  (+ a b))

^:rct/test
(comment
  ;; success case
  (add 5 3) ;=> 8
  ;; edge case: zero
  (add 0 0) ;=> 0)
```

**Syntax:** ;=> for equality, ;=>> for predicate/matcho, ;throws=>> for exceptions

**Guidelines:** cover all branches, comment each assertion, one test per branch

### Integrate RCT with clojure.test

Wrap RCT runner in a deftest so Kaocha runs both RCT and traditional tests:

```clojure
;; test/myapp/rct_test.clj
(ns myapp.rct-test
  (:require [clojure.test :refer [deftest]]
            [com.mjdowney.rich-comment-tests.test-runner :as rct]))

(deftest rct-tests
  (rct/run-tests-in-file-tree! :dirs #{"src"}))
```

### System/Integration Tests: separate namespace

Keep integration tests in `test/` using standard clojure.test:

```clojure
;; test/myapp/integration_test.clj
(ns myapp.integration-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [myapp.db :as db]))

(use-fixtures :each (fn [f] (db/with-test-conn f)))

(deftest api-integration-test
  (testing "end-to-end flow"
    (is (= expected (myapp.api/handler request)))))
```

**Separation:** RCT for pure function unit tests inline; `test/` namespace for stateful integration tests

## Capture Findings

When discovering gotchas or non-obvious behavior, add a `NOTE:` comment near the relevant code:

```clojure
;; NOTE: api-pull returns symbol keys, not keywords
(get response 'all)
```

## Code Style

- **Naming:** kebab-case, predicates end with ?, side-effect functions end with !
- **Threading:** prefer threading macros (-> ->> cond-> cond->>)
- **Conditionals:** use when-let, if-let
- **Docstrings:** every public var has a concise docstring
- **Purity:** pure functions; isolate effects at edges

## Namespace Dependencies

A namespace can only require:
1. Its **children** (e.g., `foo.bar` → `foo.bar.baz`)
2. **External libraries** (e.g., `clojure.walk`)
3. **Exception:** one `util` namespace with pure functions only

```clojure
;; ALLOWED
foo.bar       → foo.bar.baz    ; child
foo.bar       → clojure.walk   ; external
foo.bar       → project.util   ; util exception

;; NOT ALLOWED
foo.bar.a     → foo.bar.b      ; sibling
```

This prevents circular dependencies and enforces hierarchical structure.

## Subskills

- **clojurescript**: Use when working with `.cljs` files, `shadow-cljs.edn`, or frontend builds
