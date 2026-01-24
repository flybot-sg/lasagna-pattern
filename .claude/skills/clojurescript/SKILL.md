---
name: clojurescript
description: Frontend development with ClojureScript and shadow-cljs using deps.edn integration. Use when working with .cljs files, shadow-cljs.edn configuration, or setting up ClojureScript single-page applications.
---

# ClojureScript

## Prerequisites

- Clojure CLI (deps.edn)
- Node.js and npm

## Setup

### 1. deps.edn - Add :cljs alias

```clojure
:cljs
{:extra-deps {thheller/shadow-cljs {:mvn/version "2.28.22"}
              no.cjohansen/replicant {:mvn/version "2025.12.1"}
              com.cognitect/transit-cljs {:mvn/version "0.8.280"}}}
```

### 2. shadow-cljs.edn

```clojure
{:deps true
 :deps-aliases [:cljs]
 :dev-http {8080 "resources/public"}
 :builds
 {:app {:target :browser
        :output-dir "resources/public/js"
        :asset-path "/js"
        :modules {:main {:entries [your-app.core]}}
        :devtools {:after-load your-app.core/render!}}}}
```

### 3. npm setup

```bash
npm init -y
npm install --save-dev shadow-cljs
```

Add to package.json:
```json
{
  "scripts": {
    "dev": "npx shadow-cljs -A:cljs watch app",
    "release": "npx shadow-cljs -A:cljs release app"
  }
}
```

### 4. resources/public/index.html

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Your App</title>
  <link rel="stylesheet" href="/css/style.css">
</head>
<body>
  <div id="app"><p>Loading...</p></div>
  <script src="/js/main.js"></script>
</body>
</html>
```

### 5. src/your_app/core.cljs

```clojure
(ns your-app.core
  (:require [replicant.dom :as r]))

(defonce root-el (js/document.getElementById "app"))

(defn render! []
  (r/render root-el [:div "Hello World"]))

(defn init! []
  (render!))

(init!)
```

## Replicant

### Plain Hiccup, Not Reagent-Style

```clojure
;; Wrong - Reagent style
[:div [header] [footer]]

;; Correct - function calls returning hiccup
[:div (header) (footer)]
```

### Keys

```clojure
(for [item items]
  [:div {:replicant/key (:id item)} ...])
```

## Development

```bash
npm run dev
```

This starts the dev server at http://localhost:8080 with hot reload.

**REPL:** Connect to nREPL port in `.shadow-cljs/nrepl.port`, then `(shadow/repl :app)`. Exit with `:cljs/quit`.

## Testing: Use .cljc for Integration Tests

Browser-only testing is slow and catches bugs late. Move pure logic to `.cljc` for JVM testing.

### The Problem

api-pull returns **symbol** keys, not keywords:

```clojure
'{:blogs ?all}  ; pattern
{all [...]}     ; result has symbol 'all, not :all

(:all response)      ; WRONG
(get response 'all)  ; CORRECT
```

### Solution: .cljc Extraction Layer

**api.cljs** - browser-specific only:
```clojure
(ns frontend.api
  (:require [frontend.api.core :as core]))

(defn fetch! [req-data on-success on-error]
  (-> (js/fetch api-url ...)
      (.then #(on-success (core/extract-blogs %)))
      (.catch on-error)))
```

**api/core.cljc** - testable on JVM:
```clojure
(ns frontend.api.core)

(defn extract-blogs [response]
  (get response 'all))
```

**test/frontend/api_test.clj** - integration test:
```clojure
(ns frontend.api-test
  (:require [clojure.test :refer :all]
            [frontend.api.core :as api-core]
            [backend.core :as blog]
            [backend.db :as db]))

(deftest api-pull-integration-test
  (let [conn (db/create-conn "/tmp/test-db")
        api (blog/make-api conn)]
    (db/upsert-blog! conn {:blog/title "Test" :blog/slug "test" ...})
    (let [response (blog/query api '{:blogs ?all})
          blogs (api-core/extract-blogs response)]
      (is (seq blogs))
      (is (= "Test" (:blog/title (first blogs)))))
    (db/close-conn conn)))
```

### .cljs vs .cljc

| .cljs | .cljc |
|-------|-------|
| DOM manipulation | Data transformation |
| Browser APIs (fetch, localStorage) | State management logic |
| Event handlers touching DOM | Validation, business logic |
