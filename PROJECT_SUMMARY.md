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
│       ├── pullable.cljc          # Public API (failure? predicate)
│       └── pullable/
│           ├── core.cljc          # Core matcher/pattern engine
│           └── util.cljc          # Pure utility functions/macros
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

Two-phase pattern compilation:

```
Pattern → rewrite-pattern → Core Pattern → core->matcher → Matcher
             (Phase 1)                        (Phase 2)
```

**Phase 1 - Rewrite** (`rewrite-pattern`): Transforms syntax sugar to core patterns
- Uses postwalk (bottom-up, inner forms first)
- Rewrite rules are functions: `(pattern -> core-pattern | nil)`
- Example: `{:a (? :any)}` → `(? :map :a (? :any))`
- Example: `[(? :any)]` → `(? :seq (? :one (? :any)))`

**Phase 2 - Compile** (`core->matcher`): Compiles core patterns to matchers
- Uses postwalk (bottom-up)
- Each `(? :type ...)` form is validated against its defmatcher spec and compiled to a matcher function

### Key Abstractions

1. **Match Result (`ValMatchResult`)**: Holds `:val` (matched value) and `:vars` (variable bindings, default `{}`)
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
| `mzfilter` | Filter elements by predicate |
| `mzfirst` | Find first matching element |
| `mor` | First-match-wins OR |
| `mnor` | Named OR with key binding |
| `mvar` | Variable binding |
| `msub` | Value substitution |
| `mf` | Full match-result transformation |
| `wildcard` | Match anything |
| `mchain` | Chain matchers sequentially |
| `mzcollect` | Collect matching elements into a binding |
| `mterm` | Assert end of sequence |

### Compilation Functions (in `sg.flybot.pullable.core`)

| Function | Purpose |
|----------|---------|
| `rewrite-pattern` | Phase 1: Transforms syntax sugar to core patterns using rewrite rules |
| `map-rewrite` | Rewrite rule: `{:a ptn}` → `(? :map :a ptn)` |
| `vector-rewrite` | Rewrite rule: `[ptn]` → `(? :seq (? :one ptn))` |
| `core->matcher` | Phase 2: Compiles core `(? :type ...)` patterns to matchers |
| `core-rule` | Internal matcher that validates and builds core patterns |

### Pattern DSL Syntax

```clojure
;; Core form - only (? :type ...) patterns are supported
(? :type args...)

;; Examples
(? :val 5)                                    ; exact value
(? :pred even?)                               ; predicate
(? :var 'x (? :any))                          ; bind value to 'x
(? :map :a (? :var 'x (? :any)))              ; map with binding
(? :seq (? :one (? :any)) (? :term))          ; sequence [_]
(? :seq (? :repeat (? :any) :min 0))          ; sequence [_*]
(? :regex #"\d+")                             ; regex (matches strings, returns groups)
```

### Regex Matching

Regex patterns match strings and return capture groups:

```clojure
;; Basic regex - returns [full-match] or [full-match group1 group2 ...]
((core->matcher '(? :regex #"hello")) (vmr "hello"))
;=> {:val ["hello"]}

((core->matcher '(? :regex #"(\d+)-(\d+)")) (vmr "12-34"))
;=> {:val ["12-34" "12" "34"]}

;; Bind regex groups using :var
((core->matcher '(? :var parts (? :regex #"(\w+)@(\w+)"))) (vmr "user@host"))
;=> {:val "user@host" :vars {'parts ["user@host" "user" "host"]}}
```

### Case Matching

`:case` matches first successful pattern and binds the matched key:

```clojure
;; CLI option parser example
(def opt-matcher
  (core->matcher
    '(? :case opt-type
        :long-opt-with-value (? :regex #"--([a-zA-Z]+)=(.+)")
        :long-opt (? :regex #"--([a-zA-Z]+)")
        :short-opt (? :regex #"-([a-zA-Z])"))))

(opt-matcher (vmr "--name=Alice"))
;=> {:val ["--name=Alice" "name" "Alice"] :vars {'opt-type :long-opt-with-value}}
```

### Lazy vs Greedy Matching

The `:repeat` matcher supports `:greedy` option:

```clojure
;; Lazy (default): matches minimum
(? :repeat <matcher> :min 0)              ; zero or more, lazy
(? :repeat <matcher> :min 1)              ; one or more, lazy

;; Greedy: matches maximum
(? :repeat <matcher> :min 0 :greedy)      ; zero or more, greedy
(? :repeat <matcher> :min 1 :greedy)      ; one or more, greedy
```

### Maps and Sets as Predicates

Maps and sets can be used as predicates:
- **Sets** test membership: `(#{:a :b :c} val)` returns truthy if `val` is in the set
- **Maps** test key lookup: `({:a 1 :b 2} val)` returns truthy if `val` is a key in the map

```clojure
;; Set as predicate in :pred matcher
((core->matcher '(? :pred #{:active :pending})) (vmr :active))  ;=> {:val :active}
((core->matcher '(? :pred #{:active :pending})) (vmr :other))   ;=> MatchFailure

;; Set in :filter - collect matching elements
((core->matcher '(? :seq (? :filter #{:a :b} matched))) (vmr [:a :c :b :d]))
;=> {:vars {'matched [:a :b]}}
```

### Pattern Types Reference

| Type | Syntax | Description |
|------|--------|-------------|
| `:any` | `(? :any)` | Match anything (wildcard) |
| `:pred` | `(? :pred <pred>)` | Match if (pred val) is truthy. Pred can be fn, map (key lookup), or set (membership) |
| `:val` | `(? :val <value>)` | Match exact value |
| `:map` | `(? :map <key> <pattern>...)` | Match map structure |
| `:seq` | `(? :seq <matcher>...)` | Match sequence of matchers |
| `:term` | `(? :term)` | Assert end of sequence (no more elements) |
| `:var` | `(? :var <sym> <matcher>)` | Bind match to variable |
| `:one` | `(? :one <matcher>)` | Match single element in seq |
| `:optional` | `(? :optional <matcher>)` | Optional element in seq |
| `:repeat` | `(? :repeat <matcher> :min <n> [:max <n>] [:as <sym>] [:greedy])` | Repeat matcher |
| `:or` | `(? :or <matcher>...)` | Match first successful alternative |
| `:not` | `(? :not <matcher>)` | Succeed if child matcher fails |
| `:->` | `(? :-> <matcher>...)` | Chain matchers sequentially |
| `:case` | `(? :case [<sym>] <key> <pattern>...)` | Match first case, bind key to sym (patterns auto-compiled) |
| `:filter` | `(? :filter <pred> [<sym>])` | Filter sequence elements by predicate (fn/map/set) |
| `:first` | `(? :first <pred> [<sym>])` | Find first element matching predicate (fn/map/set) |
| `:sub` | `(? :sub [<matcher>] <ifn>)` | Apply ifn to transform matched value |
| `:update` | `(? :update <fn>)` | Apply fn to value (use :var to bind) |
| `:regex` | `(? :regex <pattern>)` | Match string against regex, return groups |

**Note:** Invalid patterns throw descriptive exceptions:
```clojure
(core->matcher '(? :unknown 123))
;=> ExceptionInfo: "Unknown matcher type: :unknown. Valid types: (:pred :val ...)"

(core->matcher '(? :pred "not-a-fn"))
;=> ExceptionInfo: "Invalid arguments for (? :pred ...)
;;                   Expected: (? :pred <fn>)
;;                   Error: predicate ... failed"
```

### Entry Points

```clojure
;; Low-level API (sg.flybot.pullable.core)
(require '[sg.flybot.pullable.core :as pc])

;; Compile core patterns to matchers
(def matcher (pc/core->matcher '(? :map :name (? :var 'name (? :any)))))

;; Apply matcher to data
(matcher (pc/vmr {:name "Alice" :age 30}))
;=> {:val {:name "Alice" :age 30} :vars {'name "Alice"}}

;; Check for match failure
(pc/failure? (matcher (pc/vmr "not a map")))
;=> true

;; Access MatchFailure fields for diagnostics
(let [result (matcher (pc/vmr "not a map"))]
  (when (pc/failure? result)
    {:reason (:reason result)
     :path   (:path result)
     :value  (:value result)}))
;=> {:reason "expected map, got ..." :path [] :value "not a map"}

;; Note: Sequence matching preserves collection types (vectors stay vectors, lists stay lists)
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
| `sg.flybot.pullable` | Implemented | Public API (`failure?`) |
| `sg.flybot.pullable.core` | Implemented | Core engine, matchers, MatchFailure |
| `sg.flybot.pullable.util` | Implemented | Pure utility functions/macros (e.g., `vars->`) |
