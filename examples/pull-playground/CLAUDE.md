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

### dispatch-of + pull-api (effects-as-data)

The app uses a custom dispatch pattern — NOT Replicant's built-in action system. Components close over `dispatch!` and call it directly with effect maps:

```clojure
;; Views dispatch effect maps
(dispatch! {:db state/set-loading        ; pure state update
            :pull (:pattern-text db)})   ; execute pattern

(dispatch! {:db #(state/set-mode % :remote)
            :nav :remote                 ; pushState URL
            :pull :init})                ; fetch remote data
```

### Effect types

| Effect | Value | What happens |
|--------|-------|--------------|
| `:db` | `(fn [db] db')` | `swap! app-db update root-key f` |
| `:pull` | `"pattern string"` | Execute via current mode's `:execute` |
| `:pull` | `:keyword` | Named action (`:init`, `:reset`, `:schema`) |
| `:nav` | `:sandbox` / `:remote` | `pushState` URL navigation |
| `:batch` | `(fn [db dispatcher] [...])` | Composed effects |

### pull-api — noun-based API

Data structure you can read to understand each mode's API surface:

```clojure
(def pull-api
  {:sandbox
   {:execute (fn [dispatch! _db pattern] ...)
    :reset   (fn [dispatch! _db] ...)}
   :remote
   {:execute (fn [dispatch! db pattern] ...)
    :init    (fn [dispatch! db] ...)
    :schema  (fn [dispatch! db] ...)}})
```

Leaf functions receive `dispatch!` (created by dispatch-of) and call it back with result effects. This enables recursive async flows without a separate callback system.

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
| pull-api / dispatch | `core.cljs` | No — requires browser (SCI, fetch) |
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
    ├── core.cljs            # Entry point, dispatch-of, pull-api, Transit client
    ├── state.cljc           # Pure state updaters (db → db)
    ├── sandbox.cljc         # SCI-based pattern execution
    └── views.cljc           # Replicant hiccup (dispatch! closures)
```

## deps.edn aliases

| Alias | Purpose |
|-------|---------|
| `:dev` | nREPL + cider + RCT |
| `:cljs` | shadow-cljs + SCI + replicant + transit-cljs |
| `:rct` | RCT test runner |
| `:run` | Start backend standalone |
