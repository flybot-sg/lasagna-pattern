# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**IMPORTANT: Always use the Clojure skill (`/clojure`) when working on this project.** This ensures REPL-driven development, proper idioms, and avoids common pitfalls.

**IMPORTANT: Always use `jj` instead of `git` for version control.** This is a jj/git co-located repo. Use jj commands for all operations (status, log, commit, push).

## Project Overview

flybot.pullable is a pull-based pattern matching and data transformation engine for Clojure/ClojureScript. The goal is a declarative pattern/matcher system with a small, composable core. Status: Active development; APIs are unstable.

## Commands

```bash
bb test           # Run tests (Kaocha + Rich Comment Tests)
bb dev            # Start development REPL
bb clean          # Clean target/
clojure -X:dev:test  # Direct test execution
```

## Architecture

### Source Structure
- `src/sg/flybot/pullable/core.cljc` - Core engine (~1,500 lines): matchers, rewriters, and compilation
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
{}       - Map pattern
[]       - Sequence pattern
(?x :when pred)    - Constrained match
(?x :default val)  - Default on failure
$        - Special: full matched value in match-fn body
```

## Schema Syntax Reference

Schemas validate pattern structure at compile time via `:schema` option in `compile-pattern`.

```clojure
;; Type keywords (structural validation)
:map :seq :string :number :keyword :symbol :any

;; Literal/Enum/Union/Optional - type inferred
[:= :active]              ; literal keyword
[:= 42]                   ; literal number
#{:active :inactive}      ; enum (set of allowed values)
[:or :string :number]     ; union (one of several types)
[:optional :string]       ; nullable (nil allowed)

;; Map schemas
{:name :string :age :number}     ; record (specific fields)
[:map-of :keyword :number]       ; dictionary (homogeneous k/v types)

;; Seq schemas
[:string]                        ; homogeneous seq (all elements same type)
[:tuple :number :string]         ; positional tuple

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

**Pattern construction with functions**: Use `(list '?x :when pred)` NOT `'(?x :when pred)`. The quoted form turns `pred` into a symbol instead of evaluating it to the actual function. Same applies to all pattern construction:
```clojure
;; WRONG - pred becomes symbol 'even?, not the function
'(? :pred even?)

;; CORRECT - even? is evaluated to the function
(list '? :pred even?)
```

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
