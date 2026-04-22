# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.3] - 2026-04-22

### Changed

- Replace `(satisfies? coll/Wireable v)` with a faster `instance?` on the protocol's `:on-interface` combined with a `:impls`-map lookup, avoiding the class-ancestor walk that dominates `satisfies?`'s cost (~60× speedup on large responses)
- Remove redundant `prepare-for-wire` walk from `encode-response`; Wireable conversion now happens once in `success` via `normalize-value`

### Removed

- `prepare-for-wire` (private fn) — no remaining callers

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
