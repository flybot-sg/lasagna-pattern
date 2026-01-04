# Project Summary: flybot.pullable

## Overview

A pull-based data transformation engine for Clojure/ClojureScript providing a declarative pattern/matcher system inspired by Datomic pull and GraphQL.

**Status:** Work-in-progress. APIs are unstable.

## Quick Reference

| Command | Description |
|---------|-------------|
| `bb test` or `clojure -X:dev:test` | Run tests |
| `bb dev` | Start development REPL |
| `bb clean` | Clean build artifacts |
| `clj-nrepl-eval -p <port> "<code>"` | Evaluate code via nREPL |

## Directory Structure

```
pull2/
├── src/
│   └── sg/flybot/
│       ├── pullable.cljc          # Public API (compile function)
│       └── pullable/
│           └── core.cljc          # Core matcher/pattern engine
├── test/
│   └── sg/flybot/
│       └── integrated_test.cljc
├── deps.edn                   # Clojure dependencies
├── bb.edn                     # Babashka tasks
├── CLAUDE.md                  # AI agent instructions
├── CODE_STYLE.md              # Code conventions
└── README.md
```

## Core Architecture

### Pattern System Flow

```
Pattern (Clojure data) → ptn->matcher → matcher function → ValMatchResult
```

### Key Abstractions

1. **Match Result (`ValMatchResult`)**: Holds `:val` (matched value) and `:vars` (variable bindings)
2. **Match Failure (`MatchFailure`)**: Diagnostic info on failed matches:
   - `:reason` - Human-readable failure message
   - `:matcher-type` - Keyword like `:pred`, `:map`, `:val`
   - `:path` - Vector of keys/indices showing location in data
   - `:value` - The value that failed to match
   - `:depth` - Tracks match progress (for "best failure" selection)
3. **Matcher**: Function `(mr) → ValMatchResult | MatchFailure` that transforms/validates match results
4. **Pattern**: Declarative DSL that compiles to matchers

### Matcher Primitives (in `sg.flybot.pullable.core`)

| Matcher | Purpose |
|---------|---------|
| `mpred` | Predicate-based matching |
| `mval` | Exact value matching |
| `mmap` | Map structure matching |
| `mseq` | Sequence matching |
| `mzone` | Single element in sequence |
| `mzoption` | Optional element matching |
| `mzrepeat` | Repeated element matching |
| `mor` | First-match-wins OR |
| `mnor` | Named OR with key binding |
| `mvar` | Variable binding |
| `msub` | Value substitution |
| `mf` | Full match-result transformation |
| `wildcard` | Match anything |
| `substitute` | Macro: transform match result using pattern vars |

### Pattern DSL Syntax

```clojure
;; Core form
(? :type args...)

;; Variable bindings
?x      ; bind to 'x
?x?     ; optional (0 or 1)
?x+     ; one or more (lazy - matches minimum)
?x*     ; zero or more (lazy - matches minimum)
?x+!    ; one or more (greedy - matches maximum)
?x*!    ; zero or more (greedy - matches maximum)
??x     ; sequence binding
?_      ; wildcard (no binding)

;; Examples
(? :val 5)                    ; exact value
(? :pred even?)               ; predicate
(? :var a (? :val 3))         ; bind to 'a
(? :map {:a ?a :b ?b})        ; map pattern
(? :seq [?first ?rest])       ; sequence pattern
[?head ?tail+]                ; head + rest (1 or more, lazy)
[?prefix* ?last]              ; prefix (0 or more, lazy) + last
[?head*! ?tail]               ; head (greedy) + tail
```

### Lazy vs Greedy Matching

Quantifiers (`+` and `*`) are **lazy by default** - they match the minimum required.
Add `!` suffix for **greedy** matching - matches the maximum possible.

```clojure
;; Lazy (default): first quantifier takes minimum
(query '[?a* ?b*] [1 2 3])    ;=> {a () b (1 2 3)}
(query '[?a+ ?b+] [1 2 3])    ;=> {a (1) b (2 3)}

;; Greedy: first quantifier takes maximum
(query '[?a*! ?b*] [1 2 3])   ;=> {a (1 2 3) b ()}
(query '[?a+! ?b+] [1 2 3])   ;=> {a (1 2) b (3)}
```

### Pattern Types Reference

| Type | Syntax | Description |
|------|--------|-------------|
| `:pred` | `(? :pred <fn>)` | Match if predicate returns truthy |
| `:val` | `(? :val <value>)` | Match exact value |
| `:map` | `(? :map <map>)` | Match map structure |
| `:seq` | `(? :seq [<matchers>...])` | Match sequence |
| `:var` | `(? :var <sym> <matcher>)` | Bind match to variable |
| `:1` | `(? :1 <matcher>)` | Match single element in seq |
| `:?` | `(? :? <matcher>)` | Optional element in seq |
| `:-` | `(? :- <matcher> <len> [<max-len>] [<sym>])` | Repeat matcher |

**Note:** Invalid patterns throw descriptive exceptions:
```clojure
(ptn->matcher '(? :unknown 123))
;=> ExceptionInfo: "Unknown matcher type: :unknown. Valid types: (:pred :val ...)"

(ptn->matcher '(? :pred "not-a-fn"))
;=> ExceptionInfo: "Invalid arguments for (? :pred ...)
;;                   Expected: (? :pred <fn>)
;;                   Error: predicate ... failed"
```

### Entry Points

```clojure
;; Public API (sg.flybot.pullable)
(require '[sg.flybot.pullable :as p])

(p/compile pattern)        → matcher-fn
(p/compile pattern rules)  → matcher-fn

;; Usage
(def m (p/compile '{:name ?name}))
(m {:name "Alice"})  ;=> ValMatchResult with {:vars {name "Alice"}}
(m {:name 123})      ;=> MatchFailure if pattern expects string

;; Core functions (sg.flybot.pullable.core)
(query pattern data)              → vars-map | nil
(query-with-failure pattern data) → {:vars ...} | MatchFailure

;; Low-level
(ptn->matcher pattern) → matcher-fn
(vmr value)            → ValMatchResult

;; Transformation (substitute macro)
((substitute '(+ ?x ?y)) {:vars {'x 3 'y 5}})  ;=> 8
((substitute '{:sum (* 2 ?val)}) {:vars {'val 21}})  ;=> {:sum 42}
```

## Testing

Uses **rich-comment-tests** - tests are embedded inline in source files:

```clojure
^:rct/test
(comment
  (expression) ;=> expected-value       ; equality
  (expression) ;=>> predicate           ; predicate check
  (expression) ;throws=>> #:error{...}  ; exception check
)
```

## Dependencies

- Clojure 1.12
- kaocha (test runner)
- rich-comment-tests

## Development Workflow

1. Start REPL: `bb dev`
2. Use `clj-nrepl-eval` or MCP server for evaluation
3. Make incremental changes, verify in REPL
4. Run `clj-kondo` before committing
5. Run tests: `bb test`

## Version Control

Uses **Jujutsu (jj)**, not Git. Check status with `jj status`.

## Code Conventions

- kebab-case naming
- `?` suffix for predicates, `!` for side-effects
- Threading macros (`->`, `->>`) for readability
- Pure functions; effects at edges
- Docstrings on all public vars

## Namespaces

| Namespace | Status | Purpose |
|-----------|--------|---------|
| `sg.flybot.pullable` | Implemented | Public API (`compile` function) |
| `sg.flybot.pullable.core` | Implemented | Core engine, matchers, MatchFailure |
| `sg.flybot.pullable.util` | Planned | Helpers |
| `sg.flybot.pullable.core2` | Planned | Advanced features |
