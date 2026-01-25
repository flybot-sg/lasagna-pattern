---
title: "About our Website: full-stack Clojure(Script)"
id: 8
author: loic@basecity.com
tags:
  - Blog
created-at: "2023-01-08T00:00:00.000Z"
updated-at: "2023-07-28T00:00:00.000Z"
---

This website was entirely made with Clojure and ClojureScript.

## Backend Stack

- reitit for backend routing
- muuntaja for http api format negotiation, encoding and decoding
- malli for data validation
- aleph as http server
- reitit-oauth2 for oauth2
- datalevin :as datalog db
- fun-map for systems
- lasagna-pull to precisely select from deep data structure

## Frontend

- figwheel-main for live code reloading
- hiccup for DOM representation
- reitit for frontend routing
- malli for data validation
- markdown-to-hiccup to write the content in markdown.
- re-frame a framework for building user interfaces, leveraging reagent
- re-frame-http-fx a re-frame effects handler wrapping cljs-ajax

## Main features

Employees can use their corporate google account to log in to the website. Once logged in, they can create new posts, edit existing posts and some admin users can delete posts. They have several optional configurations to customise their post such as displaying the creation/edition dates, authors/editors, add an image next to the post to illustrate it, chose different images for dark mode etc.

Everything in the sections of the website on every page is considered a post, so the whole contents can be easily modified with the UI.

## Source Code

The entire code is public and available at [flybot.sg](https://github.com/flybot-sg/flybot.sg).
