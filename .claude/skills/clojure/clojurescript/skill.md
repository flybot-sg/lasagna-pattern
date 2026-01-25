---
name: clojurescript
description: Frontend development with ClojureScript and shadow-cljs using deps.edn integration. Use when working with .cljs files, shadow-cljs.edn configuration, or setting up ClojureScript single-page applications.
---

# ClojureScript

## shadow-cljs Setup

### Basic Configuration

```clojure
;; shadow-cljs.edn
{:deps true                          ; Use deps.edn for dependencies
 :deps-aliases [:cljs]               ; Include :cljs alias from deps.edn
 :dev-http {3000 "resources/public"} ; Dev server
 :builds
 {:app {:target :browser
        :output-dir "resources/public/js"
        :asset-path "/js"
        :modules {:main {:entries [myapp.core]}}
        :devtools {:after-load myapp.core/render!}}
  :test {:target :node-test
         :output-to "target/test.js"
         :ns-regexp "myapp\\..*"
         :autorun true}}}
```

```clojure
;; deps.edn - add :cljs alias
{:aliases
 {:cljs {:extra-deps {thheller/shadow-cljs {:mvn/version "2.28.22"}}}}}
```

### Project Setup

```bash
# Install shadow-cljs locally
npm init -y
npm install --save-dev shadow-cljs

# Run commands
npx shadow-cljs watch app      # Development with hot reload
npx shadow-cljs compile app    # Production build
npx shadow-cljs compile test   # Run ClojureScript tests

# Or via Clojure CLI
clj -M:cljs -m shadow.cljs.devtools.cli watch app
clj -M:cljs -m shadow.cljs.devtools.cli compile test
```

## Macros in ClojureScript

### Critical: Self-Namespace Macro Limitation

**A namespace CANNOT use its own macros at the top level in ClojureScript.**

ClojureScript macros are compiled by JVM Clojure. When a `.cljc` file defines and uses a macro in the same namespace, the macro won't be available during the namespace's initial load.

```clojure
;; BAD - will fail in ClojureScript with:
;; "Can't take value of macro mylib.impl/my-macro"
(ns mylib.impl)
(defmacro my-macro [x] `(process ~x))
(my-macro {:foo 1})  ; ERROR!
```

### Solution 1: Convert to Function

If the macro only performs runtime operations (no code generation needed), use a function:

```clojure
;; GOOD - works in both Clojure and ClojureScript
(defn register! [type spec]
  (swap! registry* assoc type spec))
```

### Solution 2: Self-Require Macros

Add `:require-macros` for the namespace to find its own macros:

```clojure
(ns mylib.util
  #?(:cljs (:require-macros [mylib.util])))

(defmacro with-timing [& body]
  `(let [start# (System/currentTimeMillis)]
     ~@body
     (- (System/currentTimeMillis) start#)))
```

### Solution 3: Separate Namespace

Move macro calls to a namespace that requires the macro-defining namespace:

```clojure
;; mylib/macros.cljc - defines macros
(ns mylib.macros
  #?(:cljs (:require-macros [mylib.macros])))
(defmacro register! [type spec] ...)

;; mylib/registry.cljc - uses macros
(ns mylib.registry
  (:require [mylib.macros :refer [register!]]))
(register! :foo {...})  ; Works!
```

### Solution 4: Reader Conditional

Skip ClojureScript if the code is JVM-only:

```clojure
#?(:clj
   (register-schema-rule!
    (match-fn [:tuple ?types*] {:type :seq})))
```

## UI Libraries

### Replicant/Hiccup Notes

- Fragment syntax `[:<>]` may not be supported - use wrapper divs instead
- Prefer `[:div.class-name]` over `[:div {:class "class-name"}]`

```clojure
;; BAD - Replicant doesn't support fragments
[:<>
  [:div "First"]
  [:div "Second"]]

;; GOOD - use wrapper div
[:div.fragment
  [:div "First"]
  [:div "Second"]]
```

## Testing

### ClojureScript Test Target

```clojure
;; shadow-cljs.edn
{:builds
 {:test {:target :node-test
         :output-to "target/test.js"
         :ns-regexp "mylib\\..*"
         :autorun true}}}
```

### Run Tests

```bash
# Compile and run once
clj -M:cljs -m shadow.cljs.devtools.cli compile test

# Watch mode
clj -M:cljs -m shadow.cljs.devtools.cli watch test
```

### Clear Cache

When debugging macro issues or stale compilation:

```bash
rm -rf .shadow-cljs/builds/*/dev/ana/
```

## Debugging

### Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| "Can't take value of macro X" | Using macro in same namespace | See Macros section above |
| "Cannot read properties of undefined (reading 'cljs$core$IFn$_invoke$arity$N')" | Macro not expanding, called as function | Add `:require-macros` or convert to function |
| "TypeError: X is not a function" | Missing export or undefined var | Check namespace requires |

### Console Debugging

```clojure
;; Use js/console for debugging
(js/console.log "Debug:" (clj->js data))

;; Or tap> for portal/reveal
(tap> {:label "debug" :data data})
```
