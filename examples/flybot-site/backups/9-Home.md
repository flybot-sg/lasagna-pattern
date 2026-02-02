---
id: 9
title: Home
author: loic@basecity.com
tags:
  - Home
created-at: 2025-01-01T00:00:00Z
updated-at: 2025-01-01T00:00:00Z
---

## Our Language: Clojure

We use Clojure as our main programming language for development.

In short, Clojure is:

- A Functional Programming language
- A member of the Lisp family of languages
- A dynamic environment
- Having a powerful runtime polymorphism
- Simplifying multi-threaded programming
- Hosted on the JVM

## Functional Programming

We use the Data Oriented Programming (DOP) and functional programming (FP) paradigms to implement our diverse projects.

Indeed Clojure supports and relies on both of these concepts.

DOP evolves around the idiom 'Everything as data'. It is about building abstraction around basic data structures (list, maps, vectors etc.).

You can view both DOP and FP as opposition to Object Oriented Programming (OOP).

## Our Client: Golden Island

We provide technical support and solutions to clients who run 18 games in total in the platform Golden Island.

Lots of the server-side code base is written in Clojure such as user account, authentication, coins top up, message, activity, tasks/rewards, data analysis and some web pages.

## R&D Project: Clojure in Unity

Clojure can run on different platform: Java (Clojure) - JavaScript (ClojureScript) - CLR (ClojureCLR)

However, the ClojureCLR does not work with Unity as it has limited control over the generated dlls and IL2CPP for iOS is not allowed with the DLR used by ClojureCLR.

Hence the MAGIC bootstrapped compiler written in Clojure targeting the CLR. We are now able to compile Clojure libraries easily to dlls and import and use them in our Unity games.

We are currently working on:

- Improving the performance of the compiler
- Improving the deps/package/project management tool Nostrand
- Integrating Clojure directly to Unity using the architecture Entity Component System (ECS)

## Hibou Stack: Real-Time Analytics Platform

Hibou is our ongoing initiative to build a comprehensive analytics platform handling data ingestion through visualization. This flexible, client-agnostic solution allows organizations to deploy their own analytics infrastructure.

### Architecture

A monorepo with four interconnected components:

- **Analytics** - Configurable Rama module builder for custom pipelines
- **Dashboards** - Pre-built visualization module
- **API** - Authentication and interaction layer with pullable API pattern
- **UI** - Modern replicant web application

### Key Features

- Powered by Rama for distributed, real-time data processing at scale
- Smart configuration that auto-derives analytics schemas from module definitions, preventing drift
- Pure functional architecture with dependency injection and clean separation of concerns
- Client-centric design as an importable dependency with full customization capability
