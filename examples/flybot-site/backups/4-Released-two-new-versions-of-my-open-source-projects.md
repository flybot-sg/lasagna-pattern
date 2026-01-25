---
title: Released two new versions of my open-source projects
id: 4
author: lt@basecity.com
tags:
  - Blog
created-at: "2023-02-03T00:00:00.000Z"
updated-at: "2023-02-03T00:00:00.000Z"
---

## fun-map

This is one of the core components of the lasagna stack. It can turn any function into a data structure, giving programmers one more way to organize their code.

Particularly, we use it for gluing the functional code, and heavy lifting mutable components to a seeming data. Usually, we have a single map called a `system` to put all of our data, including database, and network resources in it.

This new version supports ClojureScript for the first time.

## waterfall

A library for Apache Kafka. This new version incorporated the fun-map library allowing users to write code without a deep understanding of the library we chose.
