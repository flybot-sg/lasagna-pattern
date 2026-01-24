# flybot.pullable

Work-in-progress pull-based data transformation engine for Clojure/ClojureScript. The goal is a declarative pattern/matcher system in the spirit of Datomic pull/GraphQL, with a small, composable core.

## Status
- Active development; APIs are unstable.
- Implemented: internal engine in `sg.flybot.pullable.impl`.

## Installation
Not yet released to a registry. Once a public API is stabilized, a git or tag-based dependency will be provided here.

## Features (targeted)
- Cross-platform (Clojure/JVM and ClojureScript)
- Extensible pull/pattern implementation
- Composable matchers and transformation primitives

## Components
- `sg.flybot.pullable.impl` (internal): core matchers and transformation primitives.
- Planned: `sg.flybot.pullable` (public API), `sg.flybot.pullable.util` (helpers).

## Development

### Prerequisites
- [Clojure CLI tools](https://clojure.org/guides/deps_and_cli)

### Workflow
- Follow CODE_STYLE.md for conventions.

### Running Tests
This project uses [Kaocha](https://github.com/lambdaisland/kaocha) and rich-comment-tests. Run regression tests with:

```shell
clojure -X:dev:test
```

You can also use the Babashka task:

```shell
bb test
```

## License

This project is released into the public domain using the [UNLICENSE](./UNLICENSE). This means you can copy, modify, publish, use, compile, sell, or distribute this software, either in source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any means, without attribution or other restrictions.

For more information, please refer to <http://unlicense.org/>
