# pattern

Core pattern DSL for declarative matching and transformation of Clojure data structures.

## Rationale

Extracting data from nested structures typically requires manual traversal code that's tedious to write and hard to maintain. This library provides a **pattern language** where you describe *what* you want, not *how* to get it:

- **Declarative** - Patterns mirror the shape of your data
- **Variable binding** - Extract values into named bindings
- **Constraints** - Filter matches with predicates
- **Lazy compatible** - Works with any `ILookup` implementation (databases, APIs)
- **Schema validation** - Optional compile-time validation with Malli

## Installation

```clojure
;; deps.edn (local)
{:deps {local/pattern {:local/root "../pattern"}}}
```

## Usage

### Basic matching

```clojure
(require '[sg.flybot.pullable :refer [match-fn rule apply-rules failure?]])

;; Create a matcher - returns bindings on match
(def m (match-fn '{:name ?n :age ?a} {:name ?n :age ?a}))
(m {:name "Alice" :age 30})
;=> {:name "Alice", :age 30}

;; Just extract a value
((match-fn '{:name ?n} ?n) {:name "Alice"})
;=> "Alice"

;; Use $ to access the original matched value
((match-fn '{:a ?x} (assoc $ :sum ?x)) {:a 1 :b 2})
;=> {:a 1 :b 2 :sum 1}
```

### Constraints

```clojure
;; :when - predicate constraint
(def adult (match-fn '{:age (?a :when #(>= % 18))} ?a))
(adult {:age 25})  ;=> 25
(adult {:age 10})  ;=> MatchFailure

;; :default - fallback value
((match-fn '{:name (?n :default "Anonymous")} ?n) {})
;=> "Anonymous"
```

### Sequence patterns

```clojure
;; Destructure sequences
((match-fn '[?first ?rest*] {:first ?first :rest ?rest}) [1 2 3 4])
;=> {:first 1, :rest (2 3 4)}

;; One or more
((match-fn '[?items+] ?items) [1 2 3])  ;=> (1 2 3)
((match-fn '[?items+] ?items) [])       ;=> MatchFailure

;; Optional element
((match-fn '[?a ?b?] [?a ?b]) [1])    ;=> [1 nil]
((match-fn '[?a ?b?] [?a ?b]) [1 2])  ;=> [1 2]
```

### Transformation rules

```clojure
;; Pattern → template transformation
(def double-to-add (rule '(* 2 ?x) '(+ ?x ?x)))
(double-to-add '(* 2 5))  ;=> (+ 5 5)
(double-to-add '(* 3 5))  ;=> nil (no match)

;; Apply rules recursively (bottom-up)
(def simplify-add-0 (rule '(+ 0 ?x) ?x))
(apply-rules [double-to-add simplify-add-0] '(+ 0 (* 2 y)))
;=> (+ y y)
```

### Failure handling

```clojure
(let [result ((match-fn '{:x ?x} ?x) "not a map")]
  (when (failure? result)
    {:reason (:reason result)
     :path   (:path result)
     :value  (:value result)}))
;=> {:reason "expected map", :path [], :value "not a map"}
```

## Pattern Syntax

| Pattern | Description |
|---------|-------------|
| `?x` | Bind value to `x` |
| `?_` | Wildcard (match anything, no binding) |
| `?x?` | Optional (0-1 elements) |
| `?x*` | Zero or more (lazy) |
| `?x+` | One or more (lazy) |
| `?x*!` | Zero or more (greedy) |
| `?x+!` | One or more (greedy) |
| `{}` | Map pattern |
| `[]` | Sequence pattern |
| `(?x :when pred)` | Constrained match |
| `(?x :default val)` | Default on failure |
| `{{:id 1} ?result}` | Indexed lookup (ILookup) |
| `$` | Original input (in body) |

## Map Matching

Map patterns work with both standard Clojure maps and any `ILookup` implementation:

```clojure
;; Standard map - preserves unmatched keys
((match-fn '{:a ?x} $) {:a 1 :b 2 :c 3})
;=> {:a 1 :b 2 :c 3}  ; :b and :c preserved

;; ILookup - only returns matched keys (can't enumerate)
;; Used for lazy data sources (databases, collections)

;; Indexed lookup with non-keyword keys
{{:id 1} ?user}      ; lookup by map query
{[0 1] ?cell}        ; lookup by vector key
```

## Schema Validation

Optional compile-time validation using `:schema` option:

```clojure
;; With Malli schemas (require sg.flybot.pullable.malli first)
(require '[sg.flybot.pullable.malli])
(require '[malli.core :as m])

((match-fn '{:name ?n} ?n {:schema (m/schema [:map [:name :string]])})
 {:name "alice"})
;=> "alice"

;; Indexed lookup requires :ilookup annotation
(def api-schema
  (m/schema [:map [:users [:vector {:ilookup true}
                           [:map [:id :int] [:name :string]]]]]))

;; Now both patterns are valid:
'{:users ?all}              ; LIST - always valid
'{:users {{:id 1} ?user}}   ; GET - requires :ilookup true
```

See `pattern/doc/ARCHITECTURE.md` for detailed schema documentation.

## match-fn Options

```clojure
(match-fn pattern body opts)

;; opts map:
{:schema  schema        ; Malli schema for compile-time validation
 :rules   [rule ...]    ; Custom rewrite rules (prepended to defaults)
 :only    [rule ...]    ; Use only these rules (ignore defaults)
 :resolve (fn [sym])    ; Custom symbol resolver
 :eval-fn (fn [form])}  ; Custom form evaluator
```

## Public API

### Core

| Function | Signature | Description |
|----------|-----------|-------------|
| `match-fn` | `[pattern body]` or `[pattern body opts]` | Macro: create matcher function |
| `rule` | `[pattern template]` | Macro: create transformation rule |
| `apply-rules` | `[rules data]` | Apply rules recursively (bottom-up) |
| `failure?` | `[x]` | Predicate for MatchFailure records |

### Extension (Advanced)

| Function | Signature | Description |
|----------|-----------|-------------|
| `register-var-option!` | `[key handler-fn]` | Register custom variable option (like `:when`) |
| `register-schema-rule!` | `[rule-fn]` | Register custom schema type inference |
| `get-schema-info` | `[schema]` | Query schema type and structure |

## MatchFailure

On failure, `match-fn` returns a `MatchFailure` record:

```clojure
{:reason "expected map, got string"  ; Human-readable message
 :path   [:users 0 :name]            ; Path to failure location
 :value  "bad-value"}                ; The value that failed
```

Use `failure?` to check, access fields directly for details.

## Development

All commands are run from the **repository root** (see [root README](../README.md) for full task list):

```bash
bb rct pattern     # Run RCT tests only
bb test pattern    # Run full Kaocha test suite (RCT + integration)
bb dev pattern     # Start REPL
```

For internal architecture, matcher constructors, and extension points, see `pattern/doc/ARCHITECTURE.md`.
