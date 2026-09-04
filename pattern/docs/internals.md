# Pattern Internals

The parts of `pattern` below the [README](../README.md): the core forms patterns compile to, the extension handlers, how symbols in patterns are resolved, and a few gotchas.

## Core forms

`match-fn` rewrites the sugar syntax into `(? :type ...)` forms and compiles those into matcher functions. The forms can also be written directly in a pattern:

| Form | Description |
|---|---|
| `(? :pred fn)` | Predicate match |
| `(? :val value)` | Exact value equality |
| `(? :any)` | Match anything |
| `(? :var sym matcher)` | Bind the matcher's result to `sym` |
| `(? :map k1 m1 k2 m2 ...)` | Map matcher |
| `(? :seq m1 m2 ...)` | Sequence matcher (zipper-based) |
| `(? :or m1 m2 ...)` | First successful alternative |
| `(? :not matcher)` | Negation |
| `(? :case [sym] k1 m1 ...)` | Case dispatch |
| `(? :-> m1 m2 ...)` | Chain matchers |
| `(? :sub [matcher] fn)` | Transform the value with `fn` |
| `(? :update fn)` | Apply `fn` to the value |
| `(? :regex pattern)` | Regex match, returns groups |
| `(? :filter pred [sym])` | Filter sequence elements |
| `(? :first pred [sym])` | First matching element |
| `(? :one matcher)` | One element, in sequence context |
| `(? :optional matcher)` | Optional element, in sequence context |
| `(? :repeat matcher :min N [:max N] [:as sym] [:greedy])` | Repeat with bounds |
| `(? :term)` | Assert end of sequence |

## Extension handlers

- `register-var-option!` takes a keyword and a handler `(fn [option-value] -> pattern)`. The returned pattern is chained after the base matcher and inside the variable binding, so the variable binds the chain's result rather than the raw value; this is how `:when` and `:default` are implemented. Example: `(register-var-option! :extract (fn [k] (list '? :sub k)))` enables `(?x :extract :name)`, binding `?x` to the `:name` of the matched value.
- `register-schema-rule!` takes `(fn [schema] -> {:type kw, :child-schema fn, :valid-keys set} | nil)`. `:type` is one of `:map :seq :string :number :keyword :symbol :boolean :any`, `:child-schema` maps a key or index to its sub-schema, and `:valid-keys` restricts the keys of a record schema. Rules are tried last-registered first.

## Symbol resolution

Symbols inside `:when` predicates and function forms are resolved in this order:

1. The `:resolve` / `:eval-fn` options, when given.
2. SCI, when `org.babashka/sci` is on the classpath. This is detected on CLJ only; on CLJS, SCI has to be wired through `:resolve` / `:eval-fn`.
3. `clojure.core/resolve` and `eval`, on CLJ only.

`(? :pred ...)`, which is what `:when` compiles to, is the only form that resolves its argument. On CLJS with neither SCI nor `:resolve`, compiling such a predicate throws; a pattern without one needs no resolver.

The schema rules for `[:= v]`, `[:map-of k v]`, `[:or ...]`, `[:optional s]`, and `[:tuple ...]` are defined with `match-fn` and registered on CLJ only. Type keywords, record maps, enum sets, `[elem]` sequences, and Malli schemas through `sg.flybot.pullable.malli` work on both platforms.

## Gotchas

- **`(? :term)` in hand-written sequences.** Vector patterns append it automatically. A hand-written `(? :seq ...)` must end with it, or the matcher will not require the whole sequence to be consumed.
- **`?rest*` returns a list.** Quantified bindings are lists, not vectors. Use `(vec ?rest)` in the body when a vector is needed.
- **Only `(? :pred ...)` resolves symbols.** `(? :sub ...)`, `(? :update ...)`, `(? :filter ...)` and `(? :first ...)` accept any `ifn?`, and a symbol is `ifn?`. So `(? :sub clojure.string/upper-case)` in a quoted pattern applies the *symbol* as a lookup function and yields `nil` instead of resolving it. Pass a value that is already callable: a function object, or a keyword for field extraction.
