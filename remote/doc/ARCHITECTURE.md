# CLAUDE.md

This file provides guidance to Claude Code when working with the `remote` component.

**IMPORTANT: Always use the Clojure skill (`/clojure`) when working on this project.**

## Overview

The `remote` component extends pull patterns into a remote protocol over HTTP. It enables Clojure services to expose data APIs using the pattern matching DSL from the `pattern` component.

**Design Document:** See `doc/DESIGN.md` for philosophy, decisions, and rationale.

## Commands

Run from repository root:

```bash
bb test remote    # Run tests
bb dev remote     # Start development REPL
bb clean remote   # Clean target/
```

Or directly in this directory:

```bash
clojure -X:dev:test  # Direct test execution
clojure -M:dev       # Start REPL
```

## Architecture

### Source Structure

```
src/sg/flybot/pullable/remote/
├── core.cljc       ; Request/response records, protocols
├── schema.cljc     ; Schema introspection utilities
├── handler.cljc    ; Ring handler construction
└── encoding.cljc   ; Transit encoding/decoding
```

### Key Concepts

1. **API = Lazy Data + Schema**: No resolver registry. The API is a lazy data structure (using fun-map) with schema metadata.

2. **Pattern = Input + Query**: Patterns serve dual purpose - literals are constraints/inputs, variables extract values.

3. **Transparent Mutations**: No separate mutation syntax. Reads and writes look identical to clients.

### Public API (Planned)

```clojure
(require '[sg.flybot.pullable.remote :as remote])

;; Create Ring handler
(def handler
  (remote/make-handler
    (fn [request]
      ;; Return lazy data structure with schema metadata
      (-> (make-api-for-session (:session request))
          (with-meta {:schema (schema-for-role (:role request))})))))

;; Use with Ring
(def app
  (-> handler
      (wrap-session)
      (wrap-transit-params)))
```

### Dependencies

- `pattern` - Core pattern matching (local dependency)
- `ring-core` - HTTP abstractions
- `transit-clj` - Wire format encoding

### Dev Dependencies

- `fun-map` - Lazy map implementation for building APIs
- `ring-mock` - Testing Ring handlers

## Testing

Uses Rich Comment Tests (RCT) inline in source files.

```clojure
^:rct/test
(comment
  (some-fn input) ;=> expected
  )
```

## Version Control

**Use `jj` instead of `git`** for all version control operations.
