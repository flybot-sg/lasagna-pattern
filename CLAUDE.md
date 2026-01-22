# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**IMPORTANT: Always use the Clojure skill (`/clojure`) when working on this project.** This ensures REPL-driven development, proper idioms, and avoids common pitfalls.

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

### Public API (Section 4 in core.cljc)
- `match-fn` - Create pattern-matching functions with variable bindings
- `rule` - Pattern → template transformation rules
- `compile-pattern` - Low-level pattern compilation with custom rewrite rules
- `apply-rules` - Recursive tree transformation
- `failure?` - Check for match failures
- `register-var-option!` - Extend variable syntax (:when, :default)

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

## Version control

This is a jj and git co-located repo, prefer jj command over git. 
