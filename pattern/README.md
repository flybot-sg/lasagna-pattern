# pattern

Core pattern DSL for declarative matching and transformation of Clojure data structures. Pure data matching — no I/O, no storage, no HTTP.

## Rationale

Extracting data from nested structures typically requires manual traversal code that's tedious to write and hard to maintain. This library provides a **pattern language** where you describe *what* you want, not *how* to get it:

- **Declarative** — Patterns mirror the shape of your data
- **Variable binding** — Extract values into named bindings
- **Constraints** — Filter matches with predicates and defaults
- **Lazy compatible** — Works with any `ILookup` implementation (databases, APIs, collections)
- **Schema validation** — Optional compile-time validation with Malli
- **Cross-platform** — `.cljc` throughout, runs on CLJ and CLJS

## Installation

```clojure
;; deps.edn — git dependency
{:deps
 {io.github.flybot-sg/lasagna-pattern
  {:git/url "https://github.com/flybot-sg/lasagna-pattern.git"
   :git/sha "..."
   :deps/root "pattern"}}}

;; or local (monorepo development)
{:deps {local/pattern {:local/root "../pattern"}}}
```

Clojars publication is planned — `pattern` will get its own artifact.

Only hard dependency is `org.clojure/clojure`. Optional deps: `org.babashka/sci` (sandboxed eval, required for CLJS), `metosin/malli` (schema validation).

## Usage

### Basic matching

```clojure
(require '[sg.flybot.pullable :refer [match-fn rule apply-rules failure?]])

;; Create a matcher — returns bindings on match
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
;; :when — predicate constraint
(def adult (match-fn '{:age (?a :when #(>= % 18))} ?a))
(adult {:age 25})  ;=> 25
(adult {:age 10})  ;=> MatchFailure

;; :default — fallback value
((match-fn '{:name (?n :default "Anonymous")} ?n) {})
;=> "Anonymous"
```

### Map value patterns

```clojure
;; Literal values — exact equality check
((match-fn '{:type :user} $) {:type :user :name "Alice"})
;=> {:type :user :name "Alice"}

;; Functions — predicate check
((match-fn '{:age even?} $) {:age 30})   ;=> {:age 30}
((match-fn '{:age even?} $) {:age 31})   ;=> MatchFailure

;; Sets — membership check
((match-fn '{:status #{:active :pending}} $) {:status :active})
;=> {:status :active}

;; Regex — string pattern match
((match-fn '{:phone #"(\d{3})-(\d{4})"} $) {:phone "555-1234"})
;=> {:phone "555-1234"}
```

### Sequence patterns

```clojure
;; Fixed-length destructure
((match-fn '[?a ?b] [?a ?b]) [1 2])
;=> [1 2]

;; Head + rest (zero or more)
((match-fn '[?first ?rest*] {:first ?first :rest ?rest}) [1 2 3 4])
;=> {:first 1, :rest (2 3 4)}

;; One or more — fails on empty
((match-fn '[?items+] ?items) [1 2 3])  ;=> (1 2 3)
((match-fn '[?items+] ?items) [])       ;=> MatchFailure

;; Optional element — nil when absent
((match-fn '[?a ?b?] [?a ?b]) [1])    ;=> [1 nil]
((match-fn '[?a ?b?] [?a ?b]) [1 2])  ;=> [1 2]

;; Wildcard — skip without binding
((match-fn '[?_ ?second] ?second) [1 2])  ;=> 2

;; Unification — same var must match equal values
((match-fn '[?x ?x] ?x) [1 1])  ;=> 1
((match-fn '[?x ?x] ?x) [1 2])  ;=> MatchFailure
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

### Variables

| Pattern | Description |
|---------|-------------|
| `?x` | Bind value to `x` |
| `?_` | Wildcard (match anything, no binding) |
| `?x?` | Optional (0-1 elements, sequences only) |
| `?x*` | Zero or more (lazy) |
| `?x+` | One or more (lazy) |
| `?x*!` | Zero or more (greedy) |
| `?x+!` | One or more (greedy) |

### Extended variable options

| Pattern | Description |
|---------|-------------|
| `(?x :when pred)` | Constrained match |
| `(?x :default val)` | Default on failure |
| `(?x :when pred :default val)` | Both chained (default first, then predicate) |
| `(?x* :take N)` | Take up to N elements. Pair with `?_*` for rest. |
| `(?_ :skip N)` | Skip exactly N elements |

### Structural patterns

| Pattern | Description |
|---------|-------------|
| `{}` | Map pattern (maps + ILookup) |
| `[]` | Sequence pattern (any seqable) |
| `{{:id 1} ?result}` | Indexed lookup (ILookup) |
| `$` | Original input (in `match-fn` body) |

### Map value shortcuts

| Value in pattern | Behavior |
|---------|-------------|
| `?x` | Variable binding |
| `:keyword` / `42` / `"str"` | Exact equality check |
| `even?` (function) | Predicate — `(even? val)` |
| `#{:a :b}` (set) | Membership — `(contains? #{:a :b} val)` |
| `#"regex"` | Regex match on string values |

## Map Matching

Map patterns work with both standard Clojure maps and any `ILookup` implementation:

```clojure
;; Standard map — preserves unmatched keys
((match-fn '{:a ?x} $) {:a 1 :b 2 :c 3})
;=> {:a 1 :b 2 :c 3}  ; :b and :c preserved

;; ILookup — only returns matched keys (can't enumerate)
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
'{:users ?all}              ; LIST — always valid
'{:users {{:id 1} ?user}}   ; GET — requires :ilookup true
```

Schemas also act as **visibility control** — only declared keys are accessible to patterns. Undeclared keys become effectively private.

## API Reference

### Core

| Function | Kind | Signature | Description |
|----------|------|-----------|-------------|
| `match-fn` | Macro | `[pattern body]` or `[pattern body opts]` | Create matcher function with variable bindings |
| `rule` | Macro | `[pattern template]` | Create pattern → template transformation |
| `apply-rules` | Fn | `[rules data]` | Apply rules recursively (bottom-up postwalk) |
| `failure?` | Fn | `[x]` | Predicate for MatchFailure records |

### Options (`match-fn` / `compile-pattern`)

| Option | Description |
|--------|-------------|
| `:schema` | Malli or built-in schema for compile-time validation |
| `:rules` | Custom rewrite rules (prepended to defaults) |
| `:only` | Replace all default rules with these |
| `:resolve` | Custom symbol resolver `(fn [sym])` |
| `:eval-fn` | Custom form evaluator `(fn [form])` |

### Extension

| Function | Signature | Description |
|----------|-----------|-------------|
| `register-var-option!` | `[key handler-fn]` | Register custom variable option (like `:when`) |
| `register-schema-rule!` | `[rule-fn]` | Register custom schema type inference |
| `get-schema-info` | `[schema]` | Query schema type and structure |

## Development

```bash
bb test pattern    # Run full test suite (Kaocha + RCT)
bb rct pattern     # Run RCT tests only
bb dev pattern     # Start REPL
```

See `CLAUDE.md` for architecture, internals, and extension points. See `doc/performance-analysis.md` for complexity characteristics.
