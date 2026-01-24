# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**IMPORTANT: Always use the Clojure skill (`/clojure`) when working on this project.** This ensures REPL-driven development, proper idioms, and avoids common pitfalls.

**IMPORTANT: Always use `jj` instead of `git` for version control.** This is a jj/git co-located repo. Use jj commands for all operations (status, log, commit, push).

## Project Overview

flybot.pullable is a pull-based pattern matching and data transformation engine for Clojure/ClojureScript. The goal is a declarative pattern/matcher system with a small, composable core. Status: Active development; APIs are unstable.

## Commands

Run from repository root:

```bash
bb test pattern   # Run tests (Kaocha + Rich Comment Tests)
bb dev pattern    # Start development REPL
bb clean pattern  # Clean target/
```

Or directly in this directory:

```bash
clojure -X:dev:test  # Direct test execution
clojure -M:dev       # Start REPL
```

## Architecture

### Source Structure
- `src/sg/flybot/pullable/impl.cljc` - Core engine (~2,500 lines): matchers, rewriters, and compilation
- `src/sg/flybot/pullable/util.cljc` - Pure utilities (cond-let, vars->)
- `notebook/tutorial.clj` - Interactive tutorial for learning the public API
- `notebook/cli_parser.clj` - Real-world example: CLI argument parser

### Public API (Section 5 in core.cljc)
- `match-fn` - Create pattern-matching functions with variable bindings
- `rule` - Pattern → template transformation rules
- `compile-pattern` - Low-level pattern compilation with custom rewrite rules and schema validation
- `apply-rules` - Recursive tree transformation
- `failure?` - Check for match failures
- `register-var-option!` - Extend variable syntax (:when, :default)
- `register-schema-rule!` - Extend schema vocabulary for seq types

### Core Internal Design

**Two-phase compilation:**
1. **Rewrite phase**: Transform syntax sugar to core `(? :type ...)` patterns via `rewrite-pattern`
2. **Compile phase**: Build matcher functions via `core->matcher`

**Match Result Protocol:**
- `IMatchResult` with `-fapply` and `-bind` methods
- `ValMatchResult` (success with `:val` and `:vars`)
- `MatchFailure` (failure with `:reason`, `:path`, `:value`, `:depth`)

**Naming conventions:**
- `m-` prefix: Matcher constructors (mpred, mval, mmap, mseq, mor, mcase, mnot, mchain)
- `mz-` prefix: Zipper sub-matchers (mzone, mzrepeat, mzsubseq, mzfilter, mzfirst, mzcollect, mzoption)
- `*-rewrite`: Pattern rewrite rules (extended-var-rewrite, matching-var-rewrite, map-rewrite, vector-rewrite, list-rewrite)

**Key internal functions:**
- `vmr` - Create ValMatchResult from value
- `deeper-failure` - Select most informative failure
- `nest-failure` - Add path context to failures

## Testing

Tests use Rich Comment Tests (RCT) - inline tests in `^:rct/test` comment blocks:

```clojure
^:rct/test
(comment
  (some-fn 1 2) ;=> expected-result
  (failing-fn) ;=>> failure?  ; predicate check
  )
```

Run `bb test` to execute all tests in src/.

## Pattern Syntax Reference

```clojure
?x       - Bind single value to x
?_       - Wildcard (match anything, no binding)
?x?      - Optional (0-1 elements)
?x*      - Zero or more (lazy)
?x+      - One or more (lazy)
?x*!     - Zero or more (greedy)
?x+!     - One or more (greedy)
{}       - Map pattern (works with maps and ILookup)
[]       - Sequence pattern
(?x :when pred)    - Constrained match
(?x :default val)  - Default on failure
$        - Special: original input data in match-fn body
```

### Map Matching

Map patterns support both standard Clojure maps and any `ILookup` implementation:

```clojure
;; Standard map matching (preserves unmatched keys)
(match-fn {:a ?x} ?x) ;=> extracts :a, keeps other keys in result

;; ILookup support for lazy data sources
;; Only returns matched keys (can't enumerate all keys)
(match-fn {:a ?x} ?x) ;=> works with any ILookup

;; Indexed lookup with non-keyword keys
{{:id 1} ?result}    ;=> lookup by map key
{[0 1] ?cell}        ;=> lookup by vector key
```

Map keys in patterns are literal lookup keys (not sub-patterns). This enables indexed access patterns for collections.

## Schema Syntax Reference

Schemas validate pattern structure at compile time via `:schema` option in `compile-pattern`.

```clojure
;; Type keywords (structural validation)
:map :seq :string :number :keyword :symbol :boolean :any

;; Literal/Enum/Union/Optional - type inferred
[:= :active]              ; literal keyword
[:= 42]                   ; literal number
#{:active :inactive}      ; enum (set of allowed values)
[:or :string :number]     ; union (one of several types)
[:optional :string]       ; nullable (nil allowed)

;; Map schemas
{:name :string :age :number}     ; record (all keyword keys = restricted)
[:map-of :keyword :number]       ; dictionary (homogeneous k/v types)
{:post/id :number}               ; indexed lookup key schema (non-restricting)

;; Seq schemas
[:string]                        ; homogeneous seq (all elements same type)
[:tuple :number :string]         ; positional tuple

;; Union (collection with multiple access patterns)
[:union [post-schema] {query-schema post-schema}]
;; Supports: (seq coll) -> [post...], (get coll query) -> post

;; Example usage
(compile-pattern '{:name ?n :status ?s}
                 {:schema {:name :string :status [:= :active]}})
(compile-pattern '{:x ?x :y ?y}
                 {:schema [:map-of :keyword :number]})
```

Extend with `register-schema-rule!` using `match-fn` patterns.

## Schema as Visibility Control

Schemas provide **encapsulation through declaration** - a pattern can only access fields explicitly defined in the schema. Fields omitted from the schema become effectively "private" to pattern matchers.

```clojure
;; Data has internal implementation details
(def user-data
  {:name "Alice"
   :age 30
   :_internal-id "uuid-abc123"
   :_cache {:computed-at 1234567890}})

;; Schema declares the "public interface"
(def user-schema
  {:name :string
   :age :number})

;; Patterns can only access declared fields
(compile-pattern '{:name ?n :age ?a} {:schema user-schema})
;; => OK - both keys are in schema

(compile-pattern '{:name ?n :_internal-id ?id} {:schema user-schema})
;; => REJECTED - :_internal-id not in schema
```

| Concept | Traditional OOP | Schema-based Patterns |
|---------|-----------------|----------------------|
| Public fields | `public` keyword | Keys present in schema |
| Private fields | `private` keyword | Keys omitted from schema |
| Interface definition | `interface Foo {}` | Schema map/type |
| Access violation | Compile error | `ex-info` at pattern compile time |

**Use cases:**

1. **API boundaries** - Expose stable fields, hide implementation details that may change
2. **Versioning** - Schema defines the contract guaranteed across versions
3. **Security** - Prevent patterns from extracting sensitive fields (tokens, passwords, internal state)
4. **Protocol definition** - Schema becomes a formal specification of what data shapes are queryable

This turns schemas into more than validation - they become **access control policies** for pattern-based data extraction.

## Gotchas

**Pattern construction with functions**: Quoted patterns like `'(? :pred odd?)` contain symbols, not functions. These are automatically resolved at compile time.

**SCI auto-detection**: If `org.babashka/sci` is on the classpath, it's used automatically for sandboxed evaluation. Otherwise falls back to `clojure.core/resolve` and `eval` (CLJ only).

```clojure
;; CLJ: Works automatically - symbols and fn forms are resolved
'(? :pred even?)                    ; symbol resolved to fn
'(? :pred #(zero? (mod % 3)))       ; anonymous fn evaluated

;; To enable SCI (sandboxed, works in CLJS too):
;; Just add to your deps.edn:
;;   org.babashka/sci {:mvn/version "0.10.49"}

;; Or provide custom resolver explicitly:
(def allowed {'pos? pos? 'neg? neg?})
(compile-pattern '(? :pred pos?)
                 {:resolve #(get allowed %)})
```

**Security note**: For untrusted patterns, either use SCI (sandboxed by default) or provide a restricted `:resolve` function.

## clj-kondo Configuration

Custom hooks in `.clj-kondo/` for macros: `match-fn`, `rule`, `vars->`, `cond-let`. The `defmatcher` macro is linted as `comment`.

## Version Control

**Use `jj` instead of `git` for all version control operations.**

```bash
jj status              # Check working copy changes
jj log --limit 5       # View recent commits
jj describe -m "msg"   # Set commit message for working copy
jj bookmark set main   # Move main bookmark to current commit
jj git push --bookmark main  # Push to remote
jj new                 # Create new empty commit on top
jj squash              # Squash into parent commit
``` 
