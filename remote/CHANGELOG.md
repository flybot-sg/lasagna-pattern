# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
