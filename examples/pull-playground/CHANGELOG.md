# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Releases before 0.3.0 predate this file.

## [Unreleased]

### Changed

- Deploy no longer runs `npm install`, the build has no JS dependencies
- `shadow-cljs` npm package bumped to 3.3.5, matching `deps.edn`

## [0.4.5] - 2026-04-24

### Fixed

- Sandbox mutations work again — create, update, delete and Reset all returned `invalid-collection` in the 0.4.4 build, because `Mutable` detection always returned false in ClojureScript (`lasagna-remote` 0.1.6)

## [0.4.4] - 2026-04-24

### Fixed

- Remote-mode queries for a role the session lacks report `forbidden` instead of `match-failure`, including patterns nesting more than one level past the role key (`lasagna-remote` 0.1.5)

## [0.4.3] - 2026-03-23

### Added

- "Partial success" example: one request spanning `:guest` and `:member` returns guest data alongside an `:errors` entry for the role the session lacks

## [0.4.2] - 2026-02-14

### Added

- Remote mode is reachable on narrow screens — the mode toggle was hidden below 768px, so switching to remote was impossible on a phone
- Connect strip in remote mode on narrow screens: server URL field, Connect button with a connecting state, and an inline error when the server cannot be reached

### Changed

- Single-panel tab layout now applies below 1024px instead of below 768px

### Fixed

- Bottom tab bar no longer floats over the panels — it sits in normal flow, so the last rows of a panel are not covered

## [0.4.1] - 2026-02-13

### Changed

- README renames "Local Mode" to "Sandbox Mode", matching the label in the app, and documents how to deploy the playground. The "Example Patterns" and "Demo Server Data" sections are gone — the app ships those examples itself

## [0.4.0] - 2026-02-13

### Added

- Remote mode connects to [flybot.sg](https://www.flybot.sg) as a guest by default, with no setup — it previously required a local backend started with `bb server`
- Version footer, stamped from `resources/version.edn` at deploy time

## [0.3.0] - 2026-02-11

### Added

- Interactive browser playground for the pull pattern DSL
- EDN editor with syntax highlighting and rainbow parens
- Sandbox mode: patterns and CRUD mutations run entirely in the browser against in-memory sample data, with Reset to restore it
- Remote mode: patterns run against a pull-compatible server, with the schema and sample data fetched from its `/_schema` endpoint
- Malli schema autocomplete and hover tooltips in the pattern editor
- Pre-loaded examples covering bindings, collections, sequences, constraints, and mutations
- Bottom tab bar for narrow screens
- Lasagna Pattern branding and favicon
