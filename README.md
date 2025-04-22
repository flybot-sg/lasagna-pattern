# flybot.pullable

A Clojure/ClojureScript library providing flexible, extensible pull-based data transformation operations. This library allows for declarative data access patterns similar to GraphQL and Datomic's pull API.

## Installation

Add the following dependency to your `deps.edn` file:

```clojure
{:deps
 {sg.flybot/pullable #:git{:url "https://github.com/flybot-sg/pullable"
                           :sha "current-sha-here"}}}
```

## Features

- Cross-platform support (works in both Clojure and ClojureScript)
- Extensible pull pattern implementation
- Utility functions for common pull operations
- Advanced data transformation capabilities

The library is organized into these main components:

- `sg.flybot.pullable`: Main namespace with the primary API
- `sg.flybot.pullable.util`: Utility functions to support pull operations
- `sg.flybot.pullable.core2`: Enhanced pull functionality

## Development

### Prerequisites

- [Clojure CLI tools](https://clojure.org/guides/deps_and_cli)

### Running Tests

This project uses [Kaocha](https://github.com/lambdaisland/kaocha) for testing. To run the tests:

```shell
clojure -X:test
```

## License

This project is released into the public domain using the [UNLICENSE](./UNLICENSE). This means you can copy, modify, publish, use, compile, sell, or distribute this software, either in source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any means, without attribution or other restrictions.

For more information, please refer to <http://unlicense.org/>
