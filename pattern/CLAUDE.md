# Pattern

Pull-based pattern matching and data transformation engine for Clojure/ClojureScript. Declarative patterns compile to composable matcher functions that extract, validate, and transform data.

**Status:** Active development; APIs are unstable.

## Scope

This component is **pure data matching** — no I/O, no storage, no HTTP. It takes a pattern and data, returns bindings or a failure. Other components build on top:

| Concern | Handled by |
|---|---|
| Pattern compilation, matching, transformation | **pattern** (this component) |
| CRUD collections, ILookup wrappers, mutations | `collection/` |
| HTTP serving, mutation detection, wire format | `remote/` |

Pattern knows nothing about collections, databases, or the network. It matches against any Clojure map, sequence, or `ILookup` implementation.

## Commands

```bash
bb test pattern        # Run all tests (Kaocha + RCT)
bb dev pattern         # Start nREPL with cider middleware
bb clean pattern       # Clean target/
```

## Public API

**Namespace:** `sg.flybot.pullable`

| Name | Kind | Signature | Purpose |
|---|---|---|---|
| `match-fn` | Macro | `[pattern body]` or `[pattern body opts]` | Compile pattern into a function. Binds `?vars` in body. `$` = original input. |
| `rule` | Macro | `[pattern template]` | Pattern-to-template transformation. Returns fn: data -> substituted template or nil. |
| `apply-rules` | Fn | `[rules data]` | Bottom-up postwalk: first matching rule transforms each node. |
| `failure?` | Fn | `[x]` | True if x is a `MatchFailure`. |
| `register-var-option!` | Fn | `[key handler-fn]` | Extend variable syntax (like `:when`, `:default`). |
| `register-schema-rule!` | Fn | `[rule-fn]` | Extend schema type inference for custom formats. |
| `get-schema-info` | Fn | `[schema]` | Query schema type/structure. |

**Lower-level:** `compile-pattern` in `impl.cljc` — two-phase compilation with full options.

### `match-fn` options

```clojure
{:schema  schema        ;; Malli or built-in schema for compile-time validation
 :rules   [rule ...]    ;; Custom rewrite rules (prepended to defaults)
 :only    [rule ...]    ;; Replace ALL default rules with these
 :resolve (fn [sym])    ;; Custom symbol resolver for quoted patterns
 :eval-fn (fn [form])}  ;; Custom form evaluator for fn literals
```

## Complete Pattern Syntax

### Variables

| Pattern | Name | Description |
|---|---|---|
| `?x` | Binding | Bind matched value to symbol `x` |
| `?_` | Wildcard | Match anything, no binding |
| `?x?` | Optional | 0 or 1 element (sequences only) |
| `?x*` | Zero-or-more | Collect 0+ elements, lazy |
| `?x+` | One-or-more | Collect 1+ elements, lazy |
| `?x*!` | Zero-or-more greedy | Collect 0+ elements, prefer maximum |
| `?x+!` | One-or-more greedy | Collect 1+ elements, prefer maximum |

All wildcard variants work too: `?_?`, `?_*`, `?_+`, `?_*!`, `?_+!` (same semantics, no binding).

Variable names cannot contain `?`, `+`, `*`, `!` — those are reserved suffix modifiers.

### Extended variable options

| Syntax | Description |
|---|---|
| `(?x :when pred)` | Match only if `(pred value)` is truthy |
| `(?x :default val)` | Substitute `val` when matched value is nil |
| `(?x :when pred :default val)` | Default applied first, then predicate |
| `(?x* :take N)` | Take up to N elements (greedy). Pair with `?_*` to consume rest. |
| `(?_ :skip N)` | Skip exactly N elements |

Custom options registered via `register-var-option!`.

### Structural patterns

| Pattern | Description |
|---|---|
| `{}` | Map pattern — matches maps and any `ILookup` |
| `[]` | Sequence pattern — matches any seqable |
| `$` | Original input data (available in `match-fn` body) |

### Map pattern details

```clojure
{:a ?x}                ;; keyword key: (get m :a), bind to ?x
{{:id 1} ?result}      ;; non-keyword key: indexed lookup
{:type :user}          ;; literal value: exact equality check
{:age even?}           ;; function value: predicate check
{:status #{:a :b}}     ;; set value: membership check
{:phone #"\d{3}-\d+"}  ;; regex value: pattern match
```

**Passthrough semantics:** standard maps preserve unmatched keys in the result. ILookup sources return only matched keys (can't enumerate).

Map keys in patterns are always literal lookup keys, never sub-patterns.

### Sequence pattern details

```clojure
[?a ?b]           ;; fixed-length: exactly 2 elements
[?first ?rest*]   ;; head + rest: first element + remaining as list
[?_ ?second]      ;; skip first, bind second
[?t+]             ;; one-or-more: all elements (fails if empty)
[?a ?b?]          ;; optional second element (nil if absent)
[?a* ?b*]         ;; split: lazy — ?a* gets minimum, ?b* gets rest
```

Vector patterns auto-append a terminal marker — the entire sequence must be consumed. `?rest*` binds as a list, not a vector.

**Unification:** same variable appearing multiple times must match equal values: `[?x ?x]` matches `[1 1]`, fails on `[1 2]`.

### Core pattern forms (advanced)

The `(? :type args...)` forms are the compilation target. Users can write these directly:

| Core Form | Description |
|---|---|
| `(? :pred fn)` | Predicate match |
| `(? :val value)` | Exact value equality |
| `(? :any)` | Wildcard (match anything) |
| `(? :var sym matcher)` | Bind matcher result to symbol |
| `(? :map k1 m1 k2 m2 ...)` | Map matcher |
| `(? :seq sub-matchers...)` | Sequence matcher (zipper-based) |
| `(? :or m1 m2 ...)` | First successful alternative |
| `(? :not matcher)` | Negation |
| `(? :case [sym] k1 m1 ...)` | Case dispatch |
| `(? :-> m1 m2 ...)` | Chain matchers sequentially |
| `(? :sub [matcher] fn)` | Transform val with function |
| `(? :update fn)` | Apply fn to val |
| `(? :regex pattern)` | Regex match, returns groups |
| `(? :filter pred [sym])` | Filter sequence elements |
| `(? :first pred [sym])` | Find first matching element |
| `(? :one matcher)` | Single element in sequence context |
| `(? :optional matcher)` | Optional element in sequence context |
| `(? :repeat matcher :min N :max N :as sym :greedy)` | Repeat with bounds |
| `(? :term)` | Assert end of sequence |

## Architecture

### Two-phase compilation

```
User pattern          (? :type ...) core forms       Matcher function
  {:a ?x}    ──rewrite──►  (? :map :a ...)   ──compile──►  (fn [mr] ...)
  [?a ?b*]   ──rewrite──►  (? :seq ...)      ──compile──►  (fn [mr] ...)
```

1. **Rewrite** (`rewrite-pattern`): `postwalk` transforms syntax sugar to `(? :type ...)` core forms. Default rules (in order): extended-var, matching-var, map, vector, list, set (error).
2. **Compile** (`core->matcher`): `postwalk` converts core forms to matcher closures via the `defmatcher` registry.

### Match results

- `ValMatchResult` — success: `{:val matched-value, :vars {sym value ...}}`
- `MatchFailure` — failure: `{:reason str, :matcher-type kw, :path [...], :value any, :depth int}`

Deeper failures (higher depth) are preferred — they indicate the pattern got further before failing, giving better error messages.

### Matcher primitives

A **matcher** is `(fn [match-result] -> match-result | MatchFailure)`.

| Prefix | Scope | Examples |
|---|---|---|
| `m-` | Value matchers | `mpred`, `mval`, `mmap`, `mseq`, `mor`, `mcase`, `mnot`, `mchain`, `mvar` |
| `mz-` | Zipper sub-matchers (inside `mseq`) | `mzone`, `mzrepeat`, `mzoption`, `mzfilter`, `mzfirst`, `mterm` |

### Map vs sequence internals

**Map matching (`mmap`):** Reduces over key-matcher pairs. For each key, does `(get input key)`, runs the value matcher. ILookup detected via `instance?` (CLJ) / `satisfies?` (CLJS) — starts with empty map instead of the input.

**Sequence matching (`mseq`):** Converts input to `clojure.zip/seq-zip`, traverses siblings with zipper sub-matchers. Quantifiers use `try-lengths` for backtracking. The `(? :term)` marker ensures full consumption.

### Symbol resolution

Three modes, auto-selected:
1. **SCI** — auto-detected if on classpath. Sandboxed, works in CLJS.
2. **`resolve` + `eval`** — CLJ fallback when SCI absent.
3. **Custom** — via `:resolve` / `:eval-fn` options.

Dynamic vars `*resolve-sym*` and `*eval-form*` can be bound for custom resolution.

## Schema System

Schemas validate pattern structure at compile time and control output visibility.

### Built-in schema rules

```clojure
;; Type keywords
:string :number :keyword :symbol :boolean :map :seq :any

;; Record maps (restricts keys)
{:name :string :age :number}

;; Enums (type inferred from elements)
#{:active :inactive}

;; Homogeneous sequences
[:string]               ;; seq of strings
[{:id :number}]         ;; seq of records
```

### Malli integration

Require `sg.flybot.pullable.malli` to register the Malli schema rule:

```clojure
(require '[sg.flybot.pullable.malli])
(compile-pattern '{:name ?n} {:schema (m/schema [:map [:name :string]])})
```

Malli type mapping: `:int`/`:double`/`:pos-int`/... -> `:number`, `:vector`/`:sequential`/`:set` -> `:seq`, `:maybe` unwraps, `:or` finds common type.

### ILookup and `:ilookup` annotation

Collections that support indexed lookup (non-keyword keys like `{{:id 1} ?user}`) must declare this in the schema:

```clojure
[:vector {:ilookup true} [:map [:id :int] [:name :string]]]
```

Without `:ilookup true`, schema validation rejects indexed lookup patterns at compile time.

### Schema as visibility control

Schemas act as access control — only declared keys are accessible to patterns. Undeclared keys become effectively private:

```clojure
;; Schema declares "public" fields
(def user-schema {:name :string :age :number})

;; Pattern accessing undeclared :_internal-id → rejected at compile time
(compile-pattern '{:_internal-id ?id} {:schema user-schema})
```

## Extension Points

1. **`register-var-option!`** — Add variable options like `:when`/`:default`. Handler: `(fn [option-value] -> pattern-to-chain)`.
2. **`register-schema-rule!`** — Add schema type inference for custom formats. Rule: `(fn [schema] -> {:type kw, :child-schema fn, :valid-keys set} | nil)`.
3. **Custom rewrite rules** — Via `:rules` (prepend) or `:only` (replace) options on `compile-pattern`/`match-fn`.
4. **Custom symbol resolution** — Via `:resolve`/`:eval-fn` options for sandboxed or restricted evaluation.
5. **`defmatcher`** (internal) — Register new `(? :type ...)` core pattern types.

## Cross-platform (CLJ vs CLJS)

All source files are `.cljc`. Key differences:

| Feature | CLJ | CLJS |
|---|---|---|
| Symbol resolution | SCI (auto) or `resolve`+`eval` | SCI required, or provide `:resolve`/`:eval-fn` |
| Built-in schema rules (`[:=]`, `[:map-of]`, etc.) | Registered at load | Not registered (uses `match-fn` which needs `eval`) |
| ILookup detection | `instance? clojure.lang.ILookup` | `satisfies? ILookup` |
| Malli detection | Runtime `require` | Must be in `:require` at compile time |

## Testing

Inline Rich Comment Tests (RCT) throughout `impl.cljc` + cross-platform `clojure.test` suite in `test/`.

```bash
bb test pattern   # Runs both RCT and clojure.test via Kaocha
```

Every new matcher, rewrite rule, or public function needs an RCT test.

## Files

```
src/sg/flybot/pullable.cljc           # Public API namespace (match-fn, rule, apply-rules)
src/sg/flybot/pullable/
  impl.cljc                           # Core engine (~2400 lines): matchers, rewriters, compilation
  util.cljc                           # Pure utilities (variable?, contains-variables?, vars->)
  schema.cljc                         # Schema registry, validation, output filtering
  malli.cljc                          # Optional Malli integration
test/sg/flybot/pullable_test.cljc     # Cross-platform clojure.test suite
test/sg/flybot/integrated_test.clj    # CLJ-only: wraps RCT into deftest for Kaocha
notebook/tutorial.clj                 # Interactive 17-part tutorial
notebook/cli_parser.clj               # Real-world CLI argument parser example
doc/ARCHITECTURE.md                   # Legacy CLAUDE.md (predecessor to this file)
doc/performance-analysis.md           # Complexity analysis per pattern type
```

## Dependencies

Only hard dependency is `org.clojure/clojure`. Everything else is optional:

| Dependency | When needed |
|---|---|
| `org.babashka/sci` | CLJS symbol resolution, or sandboxed eval in CLJ |
| `metosin/malli` | Schema validation with Malli types |

## Common Mistakes

### Forgetting `(? :term)` in hand-written core patterns

Vector patterns auto-append it, but if you write `(? :seq ...)` manually, you must include `(? :term)` or the matcher won't enforce full consumption.

### Expecting `?rest*` to return a vector

Quantified bindings return lists, not vectors. Use `(vec ?rest)` in the body if you need a vector.

### Using `:when` in CLJS without SCI

`:when` predicates contain symbols that need resolution. In CLJS, either add SCI to your deps or provide an explicit `:resolve` function.

### Multiple quantifiers in sequences

Each additional quantifier multiplies the backtracking space. `[?a+ ?b+ ?c+]` is O(n^2). Prefer specific patterns with `:filter` or `:first` over multiple adjacent quantifiers. See `doc/performance-analysis.md`.

### Confusing map passthrough with ILookup

Standard maps preserve unmatched keys — `(match-fn {:a ?x} $)` on `{:a 1 :b 2}` returns `{:a 1 :b 2}`. ILookup sources return only matched keys — same pattern on an ILookup returns `{:a 1}`. This difference matters when patterns are used to select data from collections vs plain maps.
