---
title: Clojure CLR experience
id: 5
author: lt@basecity.com
tags:
  - Blog
created-at: "2023-01-31T00:00:00.000Z"
updated-at: "2023-01-31T00:00:00.000Z"
---

[Clojure CLR](https://github.com/clojure/clojure-clr) is a Clojure language port on CLR(.net) platform. It was not widely used although has a long history.

```bash
dotnet tool install --global Clojure.Main
```

installs it as a dotnet tool, then we can type `Clojure.Main` to run a familiar REPL for it.

`Clojure.Main -m 'hello` will run -main function inside the hello.clj file, just like Clojure.

It uses the `CLOJURE_LOAD_PATH` environment variable to find a CLASSPATH-style loading path.

## Pending problems and possible solutions

- `-e` switch does not respect `CLOJURE_LOAD_PATH`, makes calling an arbitrary function not easy.
- no easy way to construct a full `CLOJURE_LOAD_PATH`. Thinking about using babashka to do it.
- no NREPL yet.
