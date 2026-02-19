# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
