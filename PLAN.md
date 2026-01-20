# Plan: Two-Phase Pattern Compilation

## Requirement

Implement a two-phase pattern compilation system:

1. **Phase 1 (Rewrite)**: Transform syntax sugar (like `{}` maps) into core `(? :type ...)` patterns
2. **Phase 2 (Compile)**: Compile core patterns to matchers using `core->matcher`

## Conceptual Model

- **Core matchers** `(? :type ...)` = primitive structure (the "assembly language")
- **Rewrite rules** = syntax sugar that expands to core patterns (like macros)

This is analogous to Clojure's macro system:
- Phase 1 = macroexpand (prewalk, top-down) - syntax sugar → core patterns
- Phase 2 = compile/eval (postwalk, bottom-up) - core patterns → matchers

## Understanding the Current System

### Current Architecture

```
Pattern (with rules) → core->matcher → Matcher
                           ↓
              postwalk: compile children first,
              then try rules for non-core patterns
```

The `rules` parameter to `core->matcher` contains matchers that:
1. Match a value in the pattern (e.g., `(map? x)`)
2. Transform it to a matcher (e.g., `(mmap x)`)

This works because postwalk compiles children FIRST, so when `map-rule` sees `{:a <pattern>}`, the `<pattern>` is already a matcher.

### Current map-rule

```clojure
(def map-rule
  "a map in a pattern means :map matcher"
  (mchain [(mpred map?) (msub mmap)]))
```

This is a **matcher** that:
- Succeeds if the value is a map
- Transforms it by calling `(mmap value)` where values are already matchers

### Problem with Current Approach

The current system mixes rewriting and compiling in one pass:
- No clear separation between "syntax" and "semantics"
- Can't inspect intermediate core patterns
- Rules produce matchers, not patterns

## Proposed Design

### New Architecture

```
Pattern → rewrite-pattern → Core Pattern → core->matcher → Matcher
              ↓                                   ↓
         prewalk:                            postwalk:
         apply rewrite rules                 apply defmatcher specs
         (sugar → core)                      (core → matcher)
```

### Rewrite Rules

A rewrite rule is a **function** (not a matcher) with signature:
```clojure
(fn [pattern] -> core-pattern or nil)
```

Returns:
- A core `(? :type ...)` pattern if the rule applies
- `nil` if the rule doesn't apply

Example `map-rewrite-rule`:
```clojure
(defn map-rewrite-rule
  "Rewrite {} map syntax to (? :map ...) core pattern"
  [ptn]
  (when (and (map? ptn) (not (record? ptn)))
    (list '? :map (vec (mapcat identity ptn)))))

;; {:a (? :any)} → (? :map [:a (? :any)])
```

### Rewrite Phase Implementation

```clojure
(defn rewrite-pattern
  "Rewrite syntax sugar to core patterns using prewalk"
  [ptn rewrite-rules]
  (walk/prewalk
   (fn [x]
     (or (some (fn [rule] (rule x)) rewrite-rules)
         x))
   ptn))
```

Key design decisions:
1. **Prewalk** - top-down traversal like macroexpand
2. **First match wins** - rules are tried in order
3. **Single pass** - one prewalk should suffice if rules are idempotent

### Compile Phase (Existing)

`core->matcher` already handles the compile phase:
- Postwalk traverses bottom-up
- For `(? :type ...)`, uses defmatcher spec to build matcher
- For other values, currently tries rules (this parameter becomes unused)

### Full Pipeline

```clojure
(defn compile-pattern
  "Compile a pattern to a matcher using the two-phase approach"
  [ptn rewrite-rules]
  (-> ptn
      (rewrite-pattern rewrite-rules)
      (core->matcher nil)))  ;; no rules needed in phase 2
```

## Implementation Tasks

1. **Create `rewrite-pattern` function** in `core.cljc`
2. **Create `map-rewrite-rule`** that transforms `{}` to `(? :map ...)`
3. **Update `core->matcher`** to accept empty rules (already works with nil)
4. **Create `compile-pattern`** convenience function (optional)
5. **Update tests** to verify two-phase behavior
6. **Consider deprecating** the current rule-based approach in `core->matcher`

## Final Design

### Rewrite Rule Signature

```clojure
(fn [pattern] -> core-pattern | nil)
```

A rewrite rule is a simple function that:
- Returns a core `(? :type ...)` pattern if the rule applies
- Returns `nil` if the rule doesn't apply

### `map-rewrite` - Sugar for `{}`

```clojure
(defn map-rewrite
  "Rewrite map literal {} to (? :map [...]) core pattern.
   Returns nil if not applicable."
  [x]
  (when (and (map? x) (not (record? x)))
    (list '? :map (vec (mapcat identity x)))))

;; {:a (? :any)} → (? :map [:a (? :any)])
```

### `rewrite-pattern` - Apply Rules

```clojure
(defn rewrite-pattern
  "Rewrite syntax sugar to core patterns.
   Uses prewalk to apply rules top-down (like macroexpand).
   Each rule is a fn: pattern -> core-pattern or nil"
  [ptn rules]
  (walk/prewalk
   (fn [x]
     (or (some #(% x) rules) x))
   ptn))
```

### Full Pipeline

```clojure
;; Phase 1: Rewrite (syntax sugar → core patterns)
(rewrite-pattern {:user {:name '(? :var n (? :any))}} [map-rewrite])
;; => (? :map [:user (? :map [:name (? :var n (? :any))])])

;; Phase 2: Compile (core patterns → matchers)
(core->matcher <core-pattern> nil)
```

## Verified via REPL

```clojure
;; Full pipeline test:
(let [sugar {:user {:name '(? :var n (? :any))}}
      core-ptn (rewrite-pattern sugar [map-rewrite])
      matcher (core->matcher core-ptn nil)]
  (matcher (vmr {:user {:name "Alice"}})))
;; => #ValMatchResult{:val {:user {:name "Alice"}}, :vars {n "Alice"}}
```

## Implementation Tasks

1. Add `rewrite-pattern` function to `core.cljc`
2. Add `map-rewrite` rule to `core.cljc`
3. Remove/deprecate the `rules` parameter from `core->matcher` (it only handles core patterns now)
4. Add tests for the two-phase system
