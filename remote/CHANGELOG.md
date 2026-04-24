# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.6] - 2026-04-24

### Fixed

- `direct-satisfies?` regressions from 0.1.3, breaking `Wireable` / `Mutable` detection:
  - CLJS: always returned false — a function wrapper hid the protocol symbol from CLJS's `satisfies?` macro. Fixed by making `direct-satisfies?` a macro.
  - JVM: missed types extended via an interface (e.g. Malli schemas, reify over `malli.core.Schema`) — caused transit encoding to throw on `GET /api/_schema`. Fast path now falls through `supers` on direct-class miss.

## [0.1.5] - 2026-04-24

### Fixed

- Role-gate denials with patterns nesting more than one level past the role key now return the correct `:forbidden` (e.g. 403) instead of `:match-failure` (422). Example: `{:member {:posts/history {{:post/id 1} ?v}}}` against `{:member {:error {:type :forbidden}}}` — the matcher would previously descend past the error into nil and fail with "expected map, got nil" at the deepest level, masking the auth denial

### Changed

- `execute-read` now runs three steps: walk raw `data` along pattern var-paths to catch role-gate errors, trim the pattern at those paths, match the trimmed pattern, then walk the matcher's `:val` over the **trimmed** pattern's var-bindings for errors inside materialized data. Scoping the `:val` walk to trimmed bindings prevents re-reporting step-1 errors that map passthrough leaks into `:val`. Merges `data-errs` and `val-errs` for classification
- Data walk stops at any non-map value — ILookup implementations are not probed, preserving the single-invocation guarantee on happy paths
- Contract clarified: `{:error ...}` must live in plain data. Errors returned from `ILookup.valAt` remain invisible to detection and will still surface as `:match-failure` when the pattern descends past them

### Added

- `trim-pattern` (private fn) — removes pattern branches at detected error paths, returns nil when all branches are trimmed
- `detect-read-errors` (private fn) — walks raw `data` along var-paths through plain maps only

## [0.1.4] - 2026-04-24

### Changed

- `execute-read` invokes each ILookup at most once: the compiled pattern matches against data, then a post-match walk on the matcher's `:val` surfaces per-path errors without re-pulling from collections
- Drop error-covered bindings from partial-success read responses (previously leaked as `nil`, or as the error map itself when a var's path landed exactly on the error)
- Fold read-error detection, var filtering, and all-covered classification into a single pass via new private `classify-vars`

### Removed

- `detect-read-errors` and `classify-result` (private fns) — replaced by `classify-vars`

## [0.1.3] - 2026-04-22

### Changed

- Replace `(satisfies? coll/Wireable v)` with a faster `instance?` on the protocol's `:on-interface` combined with a `:impls`-map lookup, avoiding the class-ancestor walk that dominates `satisfies?`'s cost
- Remove redundant `prepare-for-wire` walk from `encode-response`; Wireable conversion now happens once in `success` via `normalize-value`

### Removed

- `prepare-for-wire` (private fn) — no remaining callers

## [0.1.2] - 2026-03-20

### Added

- `execute-read` walks var paths through data (including through ILookup) and detects errors via `:detect` config before pattern matching
- Pattern is trimmed at error paths, compiled against remaining structure, then classified as full success, partial success, or full failure
- Partial success returns successful bindings alongside an `:errors` array for failed paths (reads only — mutations remain all-or-nothing)
- `execute-mutation` detects path-level errors (e.g. role gates returning `{:error ...}`) before attempting `mutate!`
- Helpers: `extract-var-paths`, `trim-pattern`, `detect-path-error`, `detect-read-errors`, `classify-result`, `relevant-errors`
- Comprehensive RCT and deftest coverage: partial success, all-error, nested errors, ILookup errors, custom detect-fn, exceptions, schema violations

### Changed

- Shared `make-detect-fn` extracted from `execute-mutation` for use by both read and mutation paths
- Remote spec bumped to v0.3

## [0.1.1] - 2026-02-25

### Changed

- `parse-mutation` replaced hardcoded two-case `cond` with a recursive walk over single-key keyword maps until reaching the mutation leaf `{nil-or-map value}`
- Supports mutation patterns at arbitrary nesting depth (e.g. `{:a {:role/member {:posts {nil data}}}}`)
- All existing behaviour preserved

## [0.1.0] - 2026-02-19

### Added

- Ring handler (`make-handler`, `wrap-api`) for pull-based HTTP APIs
- Mutation detection via `parse-mutation` (read vs write routing)
- Content negotiation: Transit+JSON, Transit+msgpack, EDN
- Parameter substitution (`$params`)
- Security: predicate whitelist, depth limits, schema enforcement
- Error handling with configurable detection and HTTP status mapping
- Schema introspection endpoint (`/_schema`)
- JVM client (`connect`, `schema`, `url`)
- `execute` for in-process pattern evaluation
