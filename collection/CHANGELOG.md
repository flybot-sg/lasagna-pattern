# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.3] - 2026-09-03

### Fixed

- `read-only` and `wrap-mutable` implement `Seqable` and `Counted` only when the wrapped collection implements them. Wrapping a lookup-only collection (`lookup`, or any ILookup-only type) no longer sets `seqable?` to true and then throws on `seq` (#18)

## [0.1.2] - 2026-09-03

### Added

- `validated` wrapper: Malli validation of mutation input against `:query`, `:create` and `:update` schemas. Invalid input returns `{:error {:type :invalid-mutation :message <humanized>}}` without touching the inner collection

## [0.1.0] - 2026-02-19

### Added

- `DataSource` protocol for pluggable storage backends
- `Mutable` protocol for unified CRUD mutations
- `Wireable` protocol for Transit/EDN serialization
- `collection` constructor wrapping DataSource in ILookup + Seqable + Mutable
- `read-only` wrapper to disable mutations
- `wrap-mutable` for custom mutation logic with read delegation
- `lookup` for non-enumerable keyword-value resources
- `atom-source` in-memory DataSource with `TxSource` support
- Index-based query validation
