# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0] - 2026-04-14

### Changed

- Replace custom OAuth flow with oie auth library (`sg.flybot/oie`)
- Use `oie/wrap-authenticate` with session + anonymous fallback strategy
- Use `oie-authz/has-role?` for role checking, `oie/get-identity` for identity extraction
- Use `oie-oauth2/decode-id-token` (JWT) instead of HTTP call to Google userinfo API
- Switch logout to POST-only via oie `logout-handler` (CSRF protection)
- Switch session timeout to 401 for API requests (SPA handles redirect client-side)
- Move `clj-http` from runtime dep to dev-only (no longer needed at runtime)

### Added

- `:logout` effect type in `dispatch-of` (POST via `js/fetch`, CSRF-safe)
- 401 session timeout handling in `pull!` (client-side redirect)

## [0.3.1] - 2026-03-30

### Fixed

- Fix mobile overflow on home page hero card

## [0.3.0] - 2026-03-23

### Added

- Syntax highlighting for code blocks in posts

## [0.2.5] - 2026-03-23

### Added

- Partial success support for reads (detect errors per-path, return successful branches)

## [0.2.4] - 2026-02-17

### Fixed

- Prevent post ID reuse and scope history to current entity

## [0.2.3] - 2026-02-13

### Added

- Tiered author schemas with role-based field visibility
- AOT compilation for faster container startup

## [0.2.2] - 2026-02-12

### Fixed

- Return empty map for missing roles to fix guest/member pattern matching

## [0.2.1] - 2026-02-12

### Fixed

- Pin malli to 0.16.4 to fix nil-branch pattern matching

## [0.2.0] - 2026-02-12

### Added

- Display app version in footer from version.edn
- Cache busting for static assets on redeploy
- Custom modal dialog replacing native js/confirm

### Changed

- Move UI files into core/ hierarchy and merge api.cljc
- dispatch-of effect pattern with ordered execution
- Split db.clj into domain namespaces (post, user, role)

## [0.1.10] - 2026-02-06

### Fixed

- Move tags row above content in post cards

## [0.1.9] - 2026-02-06

### Fixed

- Retract old tags/pages on update and show tags on hero cards

## [0.1.8] - 2026-02-06

### Fixed

- Ensure schema is up to date on persistent DB connect

## [0.1.7] - 2026-02-06

### Changed

- Separate :post/pages from :post/tags with dedicated schema attribute

### Fixed

- Improve post card layout with tags row, featured star, and CSS polish

## [0.1.6] - 2026-02-06

### Added

- Profile page with lazy ILookup and admin management

### Fixed

- Cache-control headers

## [0.1.5] - 2026-02-05

### Fixed

- Add replicant key to editor for proper mount on navigation

## [0.1.4] - 2026-02-05

### Added

- LOG_PUBLISHER env var for CloudWatch logging

## [0.1.3] - 2026-02-05

### Added

- UI polish with Inter font, toasts, and dark mode fixes
- Mobile navigation and UX improvements
- GitHub Actions CI/CD pipeline

### Fixed

- Scroll to top on navigation

## [0.1.2] - 2026-02-04

### Fixed

- Use full git tag for ECR image tagging

## [0.1.1] - 2026-02-04

### Fixed

- Use built-in jibbit tagger and fix CI env vars

## [0.1.0] - 2026-02-04

### Added

- Initial release: public blog with employee-authored content
- Google OAuth authentication with email pattern restriction
- Role-based access control (guest, member, admin, owner)
- Pull-pattern CRUD API with collection-based authorization
- Replicant SPA frontend
- Datahike database with history tracking
- Post CRUD with ownership enforcement
- Structured error handling in frontend
- Normalized user-post relationship with author profiles
- Tag editing UI and featured flag
- Datahike S3 backend and S3 uploads support
- Mulog structured logging
- Malli schema validation for config
- Configurable Datahike backend (mem/file/s3)
- Encrypted cookie-store for sessions

### Fixed

- OAuth2 success handler and email pattern matching
- Strip frontmatter from content

### Changed

- Migrate backend logging from Timbre to mulog
- Use fun-map associative DI pattern for system modes
