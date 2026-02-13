# Pull Playground

Live at [pattern.flybot.sg](https://pattern.flybot.sg) — interactive browser-based playground for experimenting with the pull pattern DSL.

## Rationale

Learning a new pattern language is easier with immediate feedback. This playground lets you:

- **Experiment live** - Edit patterns and data, see results instantly
- **No setup needed** - Local mode runs entirely in the browser
- **Test remote APIs** - Switch to remote mode to query a live server
- **Learn by example** - Pre-loaded patterns demonstrate common use cases

## Quick Start

All commands are run from the **repository root** (see [root README](../../README.md) for full task list).

### Local Mode (Recommended)

Pattern matching runs entirely in the browser - no backend needed.

```bash
bb serve examples/pull-playground
# Open http://localhost:3001
```

### Remote Mode

Pull data from a live server — defaults to [flybot.sg](https://www.flybot.sg), no setup needed.

```bash
bb serve examples/pull-playground
# Open http://localhost:3001, switch to "Remote"
```

The playground connects to flybot.sg as a guest (read-only, no login). You can also point it at any pull-compatible server by changing the URL and clicking Connect.

## Usage

### Sandbox Mode

1. Enter a pull pattern in the Pattern editor
2. Click Execute to see matched bindings from the in-browser sample data
3. Try mutations too — create, update, delete with Reset to restore

### Remote Mode

1. Switch to Remote mode (connects to flybot.sg by default)
2. Enter a pull pattern using the role-as-top-level API
3. Click Execute to query live blog posts

Example queries against flybot.sg:

```clojure
'{:guest {:posts ?all}}                                       ; list all posts
'{:guest {:posts {{:post/id 2} {:post/title ?t :post/tags ?tags}}}} ; select fields
'{:member ?m}                                                 ; nil — no auth
```

## Deployment

From the repo root:

```bash
bb tag examples/pull-playground
```

Reads `resources/version.edn`, creates a `pull-playground-v<version>` tag, and pushes it. CI builds the SPA, stamps the version into `index.html`, and syncs to S3 + CloudFront.

Bump `resources/version.edn` before tagging.

## Development

```bash
bb serve examples/pull-playground   # Serve UI on port 3001
bb server examples/pull-playground  # Start backend on port 8081
bb clean examples/pull-playground   # Clean build artifacts
```