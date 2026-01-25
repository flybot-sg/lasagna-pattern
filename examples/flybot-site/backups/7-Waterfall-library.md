---
title: Waterfall library
id: 7
author: lt@basecity.com
tags:
  - Blog
created-at: "2023-01-09T00:00:00.000Z"
updated-at: "2023-01-16T00:00:00.000Z"
---

Streaming data is very important for data processing; Apache Kafka is the de-factor data streaming system. Yet we need help finding a perfect library to access using Clojure. The [waterfall library](https://github.com/robertluo/waterfall) is my answer to this problem.

## Basic ideas

We embrace simplicity as programmers, the library:

- minimize its dependencies.
- has minimal API set.

## New things tried

Always try something new when I develop, and during the development of the waterfall, I tried:

- Clerk, a Clojure notebook.
- Malli function schema. As a dynamic language, Clojure can only find a few easy problems as you are coding. clj-kondo and function schemas can improve this experience.
- RCF, an alternative way of REPL-based testing. However, it is not easy to let it work along with CI, so eventually, I removed it. might give rich comment test a try.
