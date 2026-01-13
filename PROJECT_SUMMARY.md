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

Pattern compilation happens in two phases:

```
Pattern (Clojure data) → ptn->core → Core Pattern → core->matcher → Matcher function
```

**Phase 1 - Outer (`ptn->core`)**: Rewrites syntax sugar into normalized core `(? :type ...)` patterns
- `'[x]` → `'(? :seq [(? :one (? :var x (? :any))) (? :term)])`
- `'x` → `'(? :var x (? :any))`
- `#"\d+"` → `'(? :regex #"\d+")`
- `even?` → `'(? :pred even?)`

**Phase 2 - Inner (`core->matcher`)**: Compiles core patterns to matchers via defmatcher specs

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
| `ptn->matcher` | Main entry point: compiles pattern to matcher (chains ptn->core + core->matcher) |
| `ptn->core` | Phase 1: rewrites syntax sugar to core `(? :type ...)` patterns |
| `core->matcher` | Phase 2: compiles core patterns to matchers via defmatcher specs |
| `core-rule` | Matcher that validates and builds core `(? :type ...)` patterns |

### Pattern DSL Syntax

```clojure
;; Top-level patterns - symbols and literals work directly
(query 'x 3)           ;=> {x 3}     ; symbol binds value
(query 3 3)            ;=> {}        ; literal matches exactly
(query 3 4)            ;=> nil       ; literal mismatch
(query "hello" "hello") ;=> {}       ; string literal
(query :foo :foo)      ;=> {}        ; keyword literal

;; Core form
(? :type args...)

;; Variable bindings - plain symbols (preferred)
x       ; bind value to 'x
_       ; wildcard (match anything, no binding)
''foo   ; match literal symbol 'foo

;; Quantified variable bindings (in sequences)
x?      ; optional (0 or 1)
x+      ; one or more (lazy - matches minimum)
x*      ; zero or more (lazy - matches minimum)
x+!     ; one or more (greedy - matches maximum)
x*!     ; zero or more (greedy - matches maximum)
_*      ; zero or more wildcard (no binding)

;; Examples
{:a x :b y}                   ; bind :a to x, :b to y (plain symbols)
[head _ tail]                 ; head, ignore middle, tail
{:type ''response}            ; match literal symbol 'response
(? :val 5)                    ; exact value
(? :pred even?)               ; predicate
[head tail+]                  ; head + rest (1 or more, lazy)
[prefix* last]                ; prefix (0 or more, lazy) + last
#"\d+"                        ; regex (matches strings, returns groups)
```

### Regex Matching

Regex patterns automatically match strings and return capture groups:

```clojure
;; Basic regex - returns [full-match] or [full-match group1 group2 ...]
((ptn->matcher #"hello") (vmr "hello"))
;=> {:val ["hello"]}

((ptn->matcher #"(\d+)-(\d+)") (vmr "12-34"))
;=> {:val ["12-34" "12" "34"]}

;; Regex in map - transforms matched value to groups
((ptn->matcher {:phone #"(\d{3})-(\d{4})"}) (vmr {:phone "555-1234"}))
;=> {:val {:phone ["555-1234" "555" "1234"]}}

;; Regex in vector - validates string element
(query ['?name #"\d+"] ["Alice" "42"])
;=> {name "Alice"}

;; Bind regex groups using :var
(query (list '? :var 'parts (list '? :regex #"(\w+)@(\w+)")) "user@host")
;=> {parts ["user@host" "user" "host"]}
```

### Match-case with Patterns

`:match-case` supports patterns (like regex) that are automatically compiled.
Uses inline key-pattern pairs (no vector wrapper):

```clojure
;; CLI option parser example - match first successful pattern
(def opt-matcher
  (compile '(? :match-case
              :long-opt-with-value #"--([a-zA-Z]+)=(.+)"
              :long-opt #"--([a-zA-Z]+)"
              :short-opt #"-([a-zA-Z])"
              opt-type)))

(query opt-matcher "--name=Alice")
;=> {opt-type :long-opt-with-value}  ; val is ["--name=Alice" "name" "Alice"]

(query opt-matcher "--verbose")
;=> {opt-type :long-opt}             ; val is ["--verbose" "verbose"]

(query opt-matcher "-v")
;=> {opt-type :short-opt}            ; val is ["-v" "v"]
```

### Lazy vs Greedy Matching

Quantifiers (`+` and `*`) are **lazy by default** - they match the minimum required.
Add `!` suffix for **greedy** matching - matches the maximum possible.

```clojure
;; Lazy (default): first quantifier takes minimum
(query '[a* b*] [1 2 3])    ;=> {a () b (1 2 3)}
(query '[a+ b+] [1 2 3])    ;=> {a (1) b (2 3)}

;; Greedy: first quantifier takes maximum
(query '[a*! b*] [1 2 3])   ;=> {a (1 2 3) b ()}
(query '[a+! b+] [1 2 3])   ;=> {a (1 2) b (3)}
```

### Maps and Sets as Predicates

Maps and sets can be used as predicates anywhere a predicate function is accepted:
- **Sets** test membership: `(#{:a :b :c} val)` returns truthy if `val` is in the set
- **Maps** test key lookup: `({:a 1 :b 2} val)` returns truthy if `val` is a key in the map

```clojure
;; Set as predicate in :pred matcher
(query (list '? :pred #{:active :pending}) :active)  ;=> {}
(query (list '? :pred #{:active :pending}) :other)   ;=> nil

;; Map as predicate (key lookup)
(query (list '? :pred {:a 1 :b 2}) :a)  ;=> {}
(query (list '? :pred {:a 1 :b 2}) :c)  ;=> nil

;; Set in map pattern - tests membership
(query {:status #{:active :pending}} {:status :active})  ;=> {}
(query {:status #{:active :pending}} {:status :closed})  ;=> nil

;; Set in :filter - collect matching elements
(query (list '? :seq [(list '? :filter #{:a :b} 'matched)]) [:a :c :b :d])
;=> {matched [:a :b]}
```

### Pattern Types Reference

| Type | Syntax | Description |
|------|--------|-------------|
| `:any` | `(? :any)` | Match anything (wildcard) |
| `:pred` | `(? :pred <pred>)` | Match if (pred val) is truthy. Pred can be fn, map (key lookup), or set (membership) |
| `:val` | `(? :val <value>)` | Match exact value |
| `:map` | `(? :map <map>)` | Match map structure |
| `:seq` | `(? :seq [<matchers>...])` | Match sequence of matchers |
| `:term` | `(? :term)` | Assert end of sequence (no more elements) |
| `:var` | `(? :var <sym> <matcher>)` | Bind match to variable |
| `:one` | `(? :one <matcher>)` | Match single element in seq |
| `:optional` | `(? :optional <matcher>)` | Optional element in seq |
| `:repeat` | `(? :repeat <matcher> :min <n> [:max <n>] [:as <sym>] [:greedy])` | Repeat matcher |
| `:or` | `(? :or <matcher>...)` | Match first successful alternative |
| `:not` | `(? :not <matcher>)` | Succeed if child matcher fails |
| `:->` | `(? :-> <matcher>...)` | Chain matchers sequentially |
| `:match-case` | `(? :match-case <key> <pattern>... [<sym>])` | Match first case, bind key to sym (patterns auto-compiled) |
| `:filter` | `(? :filter <pred> [<sym>])` | Filter sequence elements by predicate (fn/map/set) |
| `:first` | `(? :first <pred> [<sym>])` | Find first element matching predicate (fn/map/set) |
| `:sub` | `(? :sub [<matcher>] <ifn>)` | Apply ifn to transform matched value |
| `:update` | `(? :update <fn>)` | Apply fn to value (use :var to bind) |
| `:regex` | `(? :regex <pattern>)` | Match string against regex, return groups |

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

;; Compile pattern once, reuse many times
(def m (p/compile '{:name ?name :age ?age}))

;; Multiple ways to use compiled matcher:
(m {:name "Alice" :age 30})        ;=> ValMatchResult (raw, for chaining)
(p/query m {:name "Alice" :age 30}) ;=> {name "Alice" age 30} or nil
(p/match-result m {:name "Alice"})  ;=> MatchFailure with diagnostics
(p/match! m {:name "Alice" :age 30}) ;=> {name "Alice" age 30} or throws

;; Transform: returns both updated data and bindings
(p/transform '{:count (? :update inc)} {:count 5 :name "test"})
;=> {:result {:count 6 :name "test"} :vars {}}

(p/transform! '{:x (? :pred even?)} {:x 3})  ;=> throws on mismatch

;; Also works with raw patterns (compiles each time, for REPL):
(p/query '{:x ?x} {:x 42})  ;=> {x 42}

;; Term rewriting (pattern-based transformation)
(def double->add (p/rewrite-rule '[* 2 ?x] '(+ ?x ?x)))
(double->add '(* 2 5))      ;=> (+ 5 5)
(double->add '(+ 1 2))      ;=> nil (no match)

;; Low-level (sg.flybot.pullable.core)
(ptn->matcher pattern) → matcher-fn
(vmr value)            → ValMatchResult (with :vars defaulting to {})
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
| `sg.flybot.pullable` | Implemented | Public API (`compile` function) |
| `sg.flybot.pullable.core` | Implemented | Core engine, matchers, MatchFailure |
