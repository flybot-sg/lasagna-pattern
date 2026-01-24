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

## Effects-as-Data Architecture

Separate pure logic from side effects. State functions return `{:state new-state :fx effects}`.

### File Structure

```
src/your_app/
├── state.cljc    # Pure state functions, testable on JVM
├── core.cljs     # Dispatch + effect execution
├── views.cljc    # Pure hiccup views
└── api.cljs      # Browser API client
```

### state.cljc - Pure Functions

Each state function returns `{:state ... :fx ...}`:

```clojure
(ns your-app.state)

(def initial-state {:view :list :items [] :loading? false})

;; Effect helpers
(defn- api-fx [pattern on-success]
  {:api {:pattern pattern :on-success on-success :on-error :error}})

;; State functions return {:state ... :fx ...}
(defn fetch-items [state]
  {:state (assoc state :loading? true)
   :fx (api-fx '{:items ?items} :items-fetched)})

(defn items-fetched [state items]
  {:state (assoc state :items items :loading? false)})

(defn select-item [state id]
  {:state (assoc state :view :detail :selected-id id)
   :fx {:history :push}})

(defn delete-item [state id]
  {:state state
   :fx {:confirm {:message "Delete?" :on-confirm [:delete-confirmed id]}}})

(defn delete-confirmed [state id]
  {:state (assoc state :loading? true)
   :fx (api-fx {:items {{:item/id id} nil}} :item-deleted)})

(defn set-error [state error]
  {:state (assoc state :loading? false :error (str error))})
```

### core.cljs - Dispatch Table + Effects

```clojure
(ns your-app.core
  (:require [your-app.state :as state]
            [your-app.views :as views]
            [your-app.api :as api]
            [replicant.dom :as r]))

(defonce app-state (atom state/initial-state))

(declare dispatch!)

;; Map events to state functions
(def handlers
  {:fetch-items      (fn [s _] (state/fetch-items s))
   :items-fetched    state/items-fetched
   :select-item      state/select-item
   :delete-item      state/delete-item
   :delete-confirmed state/delete-confirmed
   :error            state/set-error})

(defn- apply-handler [state event]
  (let [[event-type & args] (if (vector? event) event [event])
        handler (get handlers event-type)]
    (if handler
      (apply handler state args)
      {:state state})))

;; Execute effects
(defn- execute-effects! [state {:keys [api confirm history]}]
  (when api
    (api/pull! (:pattern api)
      #(dispatch! [(:on-success api) %])
      #(dispatch! [(:on-error api) %])))
  (when confirm
    (when (js/confirm (:message confirm))
      (dispatch! (:on-confirm confirm))))
  (when (= history :push)
    (push-history! state)))

(defn dispatch! [event]
  (let [{:keys [state fx]} (apply-handler @app-state event)]
    (reset! app-state state)
    (when fx (execute-effects! state fx))))

;; Rendering
(defonce root-el (atom nil))

(defn render! []
  (when @root-el
    (r/render @root-el (views/app-view @app-state dispatch!))))

(add-watch app-state :render (fn [_ _ _ _] (render!)))

(defn init! []
  (reset! root-el (js/document.getElementById "app"))
  (dispatch! :fetch-items)
  (render!))

(init!)
```

### views.cljc - Emit Events

```clojure
(ns your-app.views)

(defn item-card [{:keys [id title]} dispatch!]
  [:div.card {:on {:click #(dispatch! [:select-item id])}}
   [:h2 title]])

(defn item-list [{:keys [items loading?]} dispatch!]
  [:div
   [:button {:on {:click #(dispatch! :fetch-items)}} "Refresh"]
   (if loading?
     [:div "Loading..."]
     (for [item items]
       [:div {:replicant/key (:id item)}
        (item-card item dispatch!)]))])

(defn app-view [state dispatch!]
  (case (:view state)
    :list (item-list state dispatch!)
    :detail (item-detail state dispatch!)))
```

### Testing on JVM

```clojure
(deftest fetch-items-test
  (let [{:keys [state fx]} (state/fetch-items state/initial-state)]
    (is (:loading? state))
    (is (= '{:items ?items} (get-in fx [:api :pattern])))))

(deftest delete-shows-confirmation
  (let [{:keys [fx]} (state/delete-item {} 1)]
    (is (= "Delete?" (get-in fx [:confirm :message])))))
```

### Benefits

1. **Testable** - State functions are pure, run on JVM
2. **Simple** - Named functions instead of giant case statement
3. **Traceable** - Log events to debug
4. **Flexible** - Easy to add new events/effects

## Testing: Use .cljc for Pure Logic

Move pure logic to `.cljc` for JVM testing. Browser-only code stays in `.cljs`.

### api-pull Returns Symbol Keys

```clojure
'{:items ?all}  ; pattern
{all [...]}     ; result has symbol 'all, not :all

(:all response)      ; WRONG
(get response 'all)  ; CORRECT
```

### .cljs vs .cljc

| .cljs | .cljc |
|-------|-------|
| DOM manipulation | Data transformation |
| Browser APIs (fetch) | State functions |
| Effect execution | Business logic |
