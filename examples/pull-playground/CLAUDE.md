# Pull Playground

Interactive SPA for learning pull patterns. Sandbox mode runs entirely in-browser (SCI); Remote mode talks to a demo server.

## Quick Start

```bash
# Single terminal — nREPL with CLJ + CLJS support
bb dev examples/pull-playground
```

```clojure
;; In REPL: start shadow-cljs (same JVM)
(shadow.cljs.devtools.server/start!)
(shadow.cljs.devtools.api/watch :app)   ; dev server on 3001

;; Backend server (needed for Remote mode only)
(start!)       ; server on 8081
(stop!)
(restart!)
```

### Ports

| Port | Purpose |
|------|---------|
| 3001 | shadow-cljs dev server (hot reload) |
| 8081 | Backend API (Remote mode) |

## Architecture

### dispatch-of + handle-pull (effects-as-data)

The app uses a custom dispatch pattern — NOT Replicant's built-in action system. Components close over `dispatch!` and call it directly with effect maps:

```clojure
;; Views dispatch effect maps
(dispatch! {:db state/set-loading        ; pure state update
            :pull :pattern})             ; execute user's pattern

(dispatch! {:db #(state/set-mode % :remote)
            :nav :remote                 ; pushState URL
            :pull :init})                ; fetch remote data
```

### Effect types

| Effect | Value | What happens |
|--------|-------|--------------|
| `:db` | `(fn [db] db')` | `swap! app-db update root-key f` |
| `:pull` | `:keyword` | Operation (`:pattern`, `:init`, `:seed`, `:schema`, `:data`) |
| `:nav` | `:sandbox` / `:remote` | `pushState` URL navigation |

### make-executor — the ONLY mode-specific function

Returns `(fn [pattern-str on-success on-error])`. Sandbox calls `remote/execute` in-process, remote sends HTTP POST. Everything else is mode-agnostic.

### handle-pull — operation dispatch

Builds the executor once, then dispatches by operation keyword:

```clojure
(defn- handle-pull [dispatch! db op]
  (let [exec (make-executor db)]
    (case op
      :pattern ...  ; execute (:pattern-text db) via exec
      :data    ...  ; fetch all collections
      :schema  ...  ; {:schema ?s} pattern
      :seed    ...  ; {:seed {nil true}} mutation
      :init    ...  ; (handle-pull ... :data) + (handle-pull ... :schema)
      )))
```

Schema and seed are pull-able data in the store — not separate endpoints. Schema is a plain map under `:schema`, seed is a `Mutable` reify under `:seed`.

### Mobile responsive layout

Bottom tab bar (Pattern | Data | Examples) appears below 768px. CSS-only panel hiding — `mobile-hidden` class has no effect on desktop. State tracks `:active-tab` in app-db. Clicking an example auto-switches to Pattern tab.

Breakpoints: 1024px (stack panels), 768px (tab bar + single panel), 600px (compact text + touch targets).

### Sandbox store

`sandbox.cljc` is stateless — no module-level atoms. It exports constructors (`make-sources`, `make-store`) and an `execute!` function that takes store + schema as explicit args. The store is created once in `init!` and stored in app-db under `:sandbox/store`. It's a stable reference — mutations modify atom-sources in-place, so the store never needs rebuilding.

### Watcher

`dispatch!` is created once (stable reference). `add-watch` on app-db triggers re-render — no separate `render!` function.

### State layer

Pure `db → db` updater functions in `state.cljc`. Testable on JVM (no browser needed). All state lives under `:app/playground` in the app-db atom.

## Testing

Uses Rich Comment Tests (RCT). State and views are `.cljc` — tests run on JVM:

```bash
bb test examples/pull-playground
```

### What to test

| Layer | File | Testable on JVM? |
|-------|------|-----------------|
| State updaters | `state.cljc` | Yes — pure functions |
| View structure | `views.cljc` | Yes — hiccup data |
| handle-pull / dispatch | `core.cljs` | No — requires browser (SCI, fetch) |
| Sandbox eval | `sandbox.cljc` | Yes — SCI runs on JVM |

### RCT conventions

```clojure
^:rct/test
(comment
  (set-mode {:mode :sandbox :result {:data 1}} :remote)
  ;=> contains {:mode :remote :result nil}
  )
```

Every new state updater or view helper needs an RCT test.

## Key Files

```
src/sg/flybot/playground/
├── common/data.cljc        # Default sample data + schema
├── server/main.clj         # Demo backend (http-kit + remote handler)
└── ui/core/
    ├── core.cljs            # Entry point, dispatch-of, handle-pull, add-watch render
    ├── state.cljc           # Pure state updaters (db → db)
    ├── sandbox.cljc         # Stateless: constructors + execute! with explicit args
    ├── views.cljc           # Replicant hiccup (dispatch! closures)
    └── views/
        └── examples.cljc    # Pre-built example patterns (19 examples in 6 sections)
```

## deps.edn aliases

| Alias | Purpose |
|-------|---------|
| `:dev` | nREPL + cider + RCT |
| `:cljs` | shadow-cljs + SCI + replicant + transit-cljs |
| `:rct` | RCT test runner |
| `:run` | Start backend standalone |
