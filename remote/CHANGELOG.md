# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
