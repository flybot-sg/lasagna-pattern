# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Getting Started

**Always read `PROJECT_SUMMARY.md` first** when you need to understand the codebase architecture, available APIs, or implementation details. It contains:
- Directory structure and namespace organization
- Key abstractions (ValMatchResult, MatchFailure, Matcher, Pattern)
- Matcher primitives reference table
- Pattern DSL syntax examples
- Public API usage examples

## Project Overview

flybot.pullable is a work-in-progress pull-based data transformation engine for Clojure/ClojureScript. It provides a declarative pattern/matcher system inspired by Datomic pull/GraphQL, with a small, composable core. APIs are currently unstable.

## Commands

```shell
# Run tests (rich-comment-tests)
clojure -X:dev:test
bb test

# Start development REPL
bb dev

# Clean build artifacts
bb clean
```

## Architecture

**Core namespaces:**
- `sg.flybot.pullable` - Public API entry point (exports `compile` function)
- `sg.flybot.pullable.core` - Core matcher/pattern engine (cross-platform .cljc)

**Pattern system flow:**
1. Pattern (Clojure data structure) → `ptn->matcher` → matcher function
2. Matcher executes against data wrapped in `ValMatchResult` (holds `:val` and `:vars`)
3. Returns match result with variable bindings

**Key matcher primitives in core:** `mpred`, `mval`, `mmap`, `mseq`, `mzone`, `mzoption`, `mzrepeat`, `mor`, `mnor`, `mvar`, `wildcard`, `msub`, `mf`

**Pattern notation:** `(? :type args...)` forms, plus `?x`, `?x?`, `??x` for variable bindings

## Development Workflow

- Use the `clojure-mcp` MCP server for REPL evaluation during development
- Prefer small, incremental edits verified in REPL before larger changes
- Run clj-kondo locally; ensure no new warnings before committing
- Never commit directly; open a PR unless explicitly instructed

## Testing Convention

Tests use rich-comment-tests embedded inline in source files:

```clojure
^:rct/test
(comment
  (+ 5 3) ;=>
  8
  (+ 5 9) ;=>>
  even?
  (throw (ex-info "example" {:a "ok"})) ;throws=>>
  #:error{:data {:a "ok"}})
```

- `;=>` for value equality
- `;=>>` for predicate/matcho-style expectations
- `throws=>>` for exception expectations

## Code Style

- Naming: kebab-case, `?` suffix for predicates, `!` suffix for side-effects
- Prefer threading macros (`->`, `->>`) and `cond->`, `cond->>` for clarity
- Use `when-let`, `if-let` for bind-and-test; `cond-let` is a project macro for conditional bindings
- Pure functions; isolate effects at the edges
- Every public var has a concise docstring

## Namespace Dependency Rules

**Rule:** A namespace can never refer to sibling namespaces within the same project. It can only refer to:
1. Its direct children namespaces (e.g., `foo.bar` can require `foo.bar.baz`)
2. External libraries (e.g., `clojure.walk`, `clojure.zip`)

**Exception:** A project may have one `util` namespace containing only pure functions without any domain knowledge. This util namespace can be required by any namespace in the project.

**Examples:**
```
;; ALLOWED:
sg.flybot.pullable         → sg.flybot.pullable.core (child)
sg.flybot.pullable.core    → sg.flybot.pullable.core.impl (child)
any namespace              → clojure.walk (external)
any namespace              → project.util (exception - pure functions only)

;; NOT ALLOWED:
sg.flybot.pullable.foo     → sg.flybot.pullable.bar (sibling)
sg.flybot.pullable.core.a  → sg.flybot.pullable.core.b (sibling)
```

**Rationale:** This rule prevents circular dependencies and spaghetti code by enforcing a clear hierarchical structure. Parent namespaces orchestrate their children; children never reach sideways to siblings.

## Version Control

This repository uses Jujutsu (jj), not Git. Small, focused commits with imperative, present-tense 

## Clojure REPL Evaluation

The command `clj-nrepl-eval` is installed on your path for evaluating Clojure code via nREPL.

**Discover nREPL servers:**

`clj-nrepl-eval --discover-ports`

**Evaluate code:**

`clj-nrepl-eval -p <port> "<clojure-code>"`

With timeout (milliseconds)

`clj-nrepl-eval -p <port> --timeout 5000 "<clojure-code>"`

The REPL session persists between evaluations - namespaces and state are maintained.
Always use `:reload` when requiring namespaces to pick up changes.messages.
