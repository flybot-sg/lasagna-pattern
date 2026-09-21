# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Runs on [ClojureCLR](https://github.com/clojure/clojure-clr); `bb cljr-test pattern` runs both suites there. Malli and SCI stay JVM/JS only - [#34](https://github.com/flybot-sg/lasagna-pattern/issues/34).

### Fixed

- `bb test pattern` runs the clojure.test suite again, through kaocha; it had run RCT only since kaocha was dropped - [#36](https://github.com/flybot-sg/lasagna-pattern/issues/36).

## [0.1.3] - 2026-02-27

### Fixed

- Schema violations return `:schema-violation` (HTTP 403) instead of `:execution-error` (HTTP 500) — `compile-pattern` returns a failing matcher instead of throwing `ExceptionInfo`

## [0.1.2] - 2026-02-23

### Changed

- Installation documented with Clojars coordinates (`sg.flybot/lasagna-pattern`) instead of a git dependency

## [0.1.1] - 2026-02-23

### Fixed

- `sg.flybot.pullable.malli` marked `^:no-doc` — hidden from the cljdoc API listing

## [0.1.0] - 2026-02-19

### Added

- Core pattern DSL: `match-fn`, `rule`, `apply-rules`
- Variable binding (`?x`), wildcards (`?_`), optional (`?x?`), quantifiers (`?x*`, `?x+`)
- Map matching with passthrough semantics and ILookup support
- Sequence matching with zipper-based traversal and backtracking
- Extended variable options: `:when`, `:default`, `:take`, `:skip`
- Schema validation system with built-in and Malli integration
- Cross-platform CLJ/CLJS support via `.cljc`
- SCI-based sandboxed evaluation
- Extensible rewrite rules and matcher registry
