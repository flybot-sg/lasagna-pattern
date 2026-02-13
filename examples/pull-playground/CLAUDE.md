# Pull Playground

Interactive SPA for learning pull patterns. Sandbox mode runs entirely in-browser (SCI); Remote mode connects to [flybot.sg](https://www.flybot.sg) by default (or any pull-compatible server).

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

### dispatch-of + pull specs (effects-as-data)

Same architecture as flybot-site. The app uses a custom dispatch pattern — NOT Replicant's built-in action system. Components close over `dispatch!` and call it directly with effect maps:

```clojure
;; Views dispatch effect maps
(dispatch! {:db db/set-loading            ; pure db update
            :pull :pattern})             ; execute user's pattern

(dispatch! {:db #(db/set-mode % :remote)
            :nav :remote                 ; pushState URL
            :pull :init})                ; fetch remote data
```

### dispatch-of (core.cljc)

Creates a **single stable dispatch closure** via volatile self-reference — no recreation on callbacks. Effects execute in `effect-order` (`[:db :pull :nav]`) — not map iteration order.

```clojure
(def dispatch! (dispatch-of app-db root-key))
```

Top-level `def` — accessible from REPL and hot-reload.

### Effect types

| Effect | Value | What happens |
|--------|-------|--------------|
| `:db` | `(fn [db] db')` | `swap! app-db update root-key f` |
| `:pull` | `:keyword` or `{:pattern ... :then ...}` | Resolved via `pull/resolve-pull`, then dispatched by spec shape (see below) |
| `:nav` | `:sandbox` / `:remote` | `pushState` URL navigation |

The `:pull` handler in `dispatch-of` routes specs by shape:
- `{:error msg}` — sets error state
- `{:fetch url :then callback}` — GET request (used by remote `:init` for `/_schema`)
- `{:pattern data :then callback}` — executes via `make-executor` (sandbox in-process, remote HTTP POST)

### Pull specs (pull.cljc) — data-driven API operations

Pull operations are **data, not imperative code**. Each named operation resolves to `{:pattern ... :then ...}`:

```clojure
;; Named specs resolve via (pull/resolve-pull op db)
:init     ; Mode-aware: sandbox → pull pattern for data + schema;
          ;             remote → GET /_schema for schema + sample data
:pattern  ; User's editor text, parsed to data. Mutations update snapshot directly.
:data     ; Read all collections
:schema   ; {:schema ?s}
:seed     ; {:seed {nil true}} → chains to :data
```

**Mutation responses are used directly.** After a mutation, `apply-mutation-result` merges the response into the data snapshot — no re-fetch. This demonstrates the core pull pattern round-trip design.

**`:then` receives response, returns effect map:**
```clojure
:then (fn [result]
        {:db #(-> % (db/set-result result)
                    (db/apply-mutation-result pattern result))})
```

### make-executor — the ONLY mode-specific function (pattern execution)

Returns `(fn [pattern on-success on-error])`. Takes pattern as **data** (not string). Sandbox calls `remote/execute` in-process (deferred via `queueMicrotask` to prevent recursive dispatch), remote sends HTTP POST. Everything else is mode-agnostic.

Initialization (`:init`) is also mode-aware but routes differently: sandbox pulls data + schema via pattern, remote fetches schema + sample via GET `/_schema`. See `resolve-pull :init` in `pull.cljc`.

### Mobile responsive layout

Bottom tab bar (Pattern | Data | Examples) appears below 768px. CSS-only panel hiding — `mobile-hidden` class has no effect on desktop. State tracks `:active-tab` in app-db. Clicking an example auto-switches to Pattern tab.

Breakpoints: 1024px (stack panels), 768px (tab bar + single panel), 600px (compact text + touch targets).

### Sandbox store

`sandbox.cljc` is stateless — no module-level atoms. It exports constructors (`make-sources`, `make-store`) and an `execute!` function that takes store, schema, and **pattern data** as explicit args. The store is created once in `init!` and stored in app-db under `:sandbox/store`. It's a stable reference — mutations modify atom-sources in-place, so the store never needs rebuilding. The sandbox store includes sample data alongside the schema (same as the server).

### DB layer

Pure `db → db` updater functions in `db.cljc`. Testable on JVM (no browser needed). All state lives under `:app/playground` in the app-db atom. Includes `apply-mutation-result` for merging mutation responses into the data snapshot.

## Testing

Uses Rich Comment Tests (RCT). State, pull specs, and views are `.cljc` — tests run on JVM:

```bash
bb test examples/pull-playground
```

### What to test

| Layer | File | Testable on JVM? |
|-------|------|-----------------|
| DB updaters | `db.cljc` | Yes — pure functions |
| Pull specs | `pull.cljc` | Yes — pure data |
| View structure | `views.cljc` | Yes — hiccup data |
| dispatch-of | `core.cljc` | Partially — transit + format-error testable on JVM |
| Sandbox eval | `sandbox.cljc` | Yes — SCI runs on JVM |

### RCT conventions

Tests go directly below the function they test — they serve as documentation:

```clojure
(defn set-mode [db mode] ...)

^:rct/test
(comment
  (let [db (set-mode {:mode :sandbox :result {:data 1}} :remote)]
    [(:mode db) (:result db)])
  ;=> [:remote nil]
  nil)
```

Every new db updater, pull spec, or view helper needs an RCT test.

## Key Files

```
src/sg/flybot/playground/
├── common/data.cljc        # Default sample data + schema
├── server/main.clj         # Demo backend (http-kit + remote handler)
└── ui/core/
    ├── core.cljc            # Entry point, dispatch-of, make-executor, init
    ├── pull.cljc            # Pull spec definitions (pattern + :then as data)
    ├── db.cljc              # Pure db updaters (db → db) + apply-mutation-result
    ├── sandbox.cljc         # Stateless: constructors + execute! with explicit args
    ├── views.cljc           # Replicant hiccup (dispatch! closures)
    └── views/
        └── examples.cljc    # Pre-built example patterns: sandbox (20) + remote (11)
```

## deps.edn aliases

| Alias | Purpose |
|-------|---------|
| `:dev` | nREPL + cider + RCT |
| `:cljs` | shadow-cljs + SCI + replicant + transit-cljs |
| `:rct` | RCT test runner |
| `:run` | Start backend standalone |
