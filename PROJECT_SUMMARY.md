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

1. **Match Result (`ValMatchResult`)**: Holds `:val` (matched value) and `:vars` (variable bindings, default `{}`)
2. **Match Failure (`MatchFailure`)**: Diagnostic info on failed matches:
   - `:reason` - Human-readable failure message
   - `:matcher-type` - Keyword like `:pred`, `:map`, `:val`
   - `:path` - Vector of keys/indices showing location in data
   - `:value` - The value that failed to match
   - `:depth` - Tracks match progress (for "best failure" selection)
3. **Matcher**: Function `(mr) → ValMatchResult | MatchFailure` that transforms/validates match results
4. **Pattern**: Declarative DSL that compiles to matchers
5. **IMatcher Protocol**: Provides `-query`, `-match-result`, `-match!` methods. Implemented by `CompiledMatcher` and extended to raw patterns (maps, vectors, lists)

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
| `substitute` | Macro: transform match result using pattern vars (evaluates result) |
| `substitute-form` | Macro: substitute vars and return unevaluated form |
| `substitute-vars` | Runtime var substitution (works with dynamic patterns) |
| `mchain` | Chain matchers sequentially |
| `mzcollect` | Collect matching elements into a binding |

### Pattern DSL Syntax

```clojure
;; Core form
(? :type args...)

;; Variable bindings (symbol form)
?x      ; bind to 'x
?x?     ; optional (0 or 1)
?x+     ; one or more (lazy - matches minimum)
?x*     ; zero or more (lazy - matches minimum)
?x+!    ; one or more (greedy - matches maximum)
?x*!    ; zero or more (greedy - matches maximum)
?_      ; wildcard (no binding)

;; Variable bindings (list form) - more control
(?x)              ; bind single value to x
(?x even?)        ; bind single value with predicate
(?x [2 4])        ; bind 2-4 elements
(?x [1 0])        ; bind 1+ elements (0 = unbounded)
(?x even? [2 3] !) ; greedy, 2-3 elements with predicate

;; Examples
(? :val 5)                    ; exact value
(? :pred even?)               ; predicate
(? :var a (? :val 3))         ; bind to 'a
(? :map {:a ?a :b ?b})        ; map pattern
(? :seq [?first ?rest])       ; sequence pattern
[?head ?tail+]                ; head + rest (1 or more, lazy)
[?prefix* ?last]              ; prefix (0 or more, lazy) + last
[?head*! ?tail]               ; head (greedy) + tail
#"\d+"                        ; regex (matches strings, returns groups)
```

### Regex Matching

Regex patterns automatically match strings and return capture groups:

```clojure
;; Basic regex - returns [full-match] or [full-match group1 group2 ...]
((ptn->matcher #"hello" core-rules) (vmr "hello"))
;=> {:val ["hello"]}

((ptn->matcher #"(\d+)-(\d+)" core-rules) (vmr "12-34"))
;=> {:val ["12-34" "12" "34"]}

;; Regex in map - transforms matched value to groups
((ptn->matcher {:phone #"(\d{3})-(\d{4})"} core-rules) (vmr {:phone "555-1234"}))
;=> {:val {:phone ["555-1234" "555" "1234"]}}

;; Regex in vector - validates string element
(query ['?name #"\d+"] ["Alice" "42"])
;=> {name "Alice"}

;; Bind regex groups with (? :regex pattern sym)
(query (list '? :regex #"(\w+)@(\w+)" 'parts) "user@host")
;=> {parts ["user@host" "user" "host"]}
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

### List-form Named Variables

For more control over matching, use the list form: `(?x pred? [min max]? !?)`

| Form | Description |
|------|-------------|
| `(?x)` | Single value, bind to x |
| `(?x even?)` | Single value with predicate |
| `(?x [2 4])` | 2-4 elements (lazy) |
| `(?x [1 0])` | 1+ elements (max=0 means unbounded) |
| `(?x even? [2 3])` | 2-3 elements, each must satisfy predicate |
| `(?x [0 5] !)` | 0-5 elements (greedy) |

```clojure
;; Single value with predicate
(query '[(?x even?)] [4])          ;=> {x 4}
(query '[(?x even?)] [3])          ;=> nil

;; Subsequence with length range
(query '[(?x [2 4])] [1 2 3])      ;=> {x (1 2 3)}
(query '[(?x [2 4])] [1])          ;=> nil (too short)

;; With predicate and length
(query '[(?x even? [2 3])] [2 4 6]) ;=> {x (2 4 6)}

;; Lazy vs greedy
(query '[(?x [0 5]) ?y*] [1 2 3])   ;=> {x () y (1 2 3)}    ; lazy
(query '[(?x [0 5] !) ?y*] [1 2 3]) ;=> {x (1 2 3) y ()}    ; greedy
```

### Pattern Types Reference

| Type | Syntax | Description |
|------|--------|-------------|
| `:pred` | `(? :pred <fn> [<args>...])` | Match if (fn args... val) is truthy |
| `:val` | `(? :val <value>)` | Match exact value |
| `:map` | `(? :map <map>)` | Match map structure |
| `:seq` | `(? :seq [<matchers>...] [:min <n>] [:max <n>] [:as <sym>] [:greedy])` | Match sequence (optionally repeated) |
| `:var` | `(? :var <sym> <matcher>)` | Bind match to variable |
| `:one` | `(? :one <matcher>)` | Match single element in seq |
| `:optional` | `(? :optional <matcher>)` | Optional element in seq |
| `:repeat` | `(? :repeat <matcher> :min <n> [:max <n>] [:as <sym>] [:greedy])` | Repeat matcher |
| `:or` | `(? :or <matcher>...)` | Match first successful alternative |
| `:not` | `(? :not <matcher>)` | Succeed if child matcher fails |
| `:->` | `(? :-> <matcher>...)` | Chain matchers sequentially |
| `:match-case` | `(? :match-case [<key> <matcher>...] [<sym>])` | Match first case, bind key to sym |
| `:filter` | `(? :filter <pred> [<sym>])` | Filter sequence elements by predicate |
| `:first` | `(? :first <pred> [<sym>])` | Find first element matching predicate |
| `:sub` | `(? :sub <fn> [<matcher>])` | Apply fn to transform matched value |
| `:regex` | `(? :regex <pattern> [<sym>])` | Match string against regex, return groups |

**Variable References in Args:** Use `$var` syntax in `:pred` args to reference bound variables:
```clojure
;; (< value-of-a current-val)
(? :pred < $a)
```

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

;; Also works with raw patterns (compiles each time, for REPL):
(p/query '{:x ?x} {:x 42})  ;=> {x 42}

;; Term rewriting (pattern-based transformation)
(def double->add (p/rewrite-rule '[* 2 ?x] '(+ ?x ?x)))
(double->add '(* 2 5))      ;=> (+ 5 5)
(double->add '(+ 1 2))      ;=> nil (no match)

;; Runtime variable substitution
(p/substitute-vars '(+ ?x ?y) {'x 3 'y 5})  ;=> (+ 3 5)

;; Low-level (sg.flybot.pullable.core)
(ptn->matcher pattern) → matcher-fn
(vmr value)            → ValMatchResult (with :vars defaulting to {})
;; Note: Sequence matching preserves collection types (vectors stay vectors, lists stay lists)

;; Transformation macros
((substitute '(+ ?x ?y)) {:vars {'x 3 'y 5}})  ;=> 8  (evaluates result)
((substitute-form '(* 2 ?x)) {:vars {'x 5}})   ;=> (* 2 5)  (returns unevaluated form)
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
