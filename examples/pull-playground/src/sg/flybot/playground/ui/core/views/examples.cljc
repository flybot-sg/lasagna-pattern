(ns sg.flybot.playground.ui.core.views.examples
  "Pre-built example patterns for the playground.")

(def examples
  [;; --- Basics: binding variables from plain maps ---
   {:name "Bind a value"
    :description "Bind an entire map to a single variable"
    :pattern "{:config ?cfg}"}

   {:name "Nested binding"
    :description "Extract a value from inside a nested map"
    :pattern "{:config {:features ?f}}"}

   {:name "Deep nesting"
    :description "Extract multiple values from deeply nested maps"
    :pattern "{:config {:features {:dark-mode ?dm :notifications ?n}}}"}

   ;; --- Collections: ILookup-based data sources ---
   {:name "List collection"
    :description "Read all items from a collection"
    :pattern "{:users ?all}"}

   {:name "Lookup by ID"
    :description "Find a specific item using indexed lookup"
    :pattern "{:users {{:id 2} ?user}}"}

   {:name "Select fields"
    :description "Extract only specific fields from a lookup"
    :pattern "{:users {{:id 1} {:name ?name :role ?role}}}"}

   ;; --- Modifiers: refine what variables match ---
   {:name "Wildcard (?_)"
    :description "Match name without binding — only extract email"
    :pattern "{:users {{:id 1} {:name ?_ :email ?email}}}"}

   {:name "Default value"
    :description "Provide a fallback when a key is missing"
    :pattern "{:config {:debug (?d :default false)}}"}

   {:name "Constraint (:when)"
    :description "Match only if the value passes a predicate"
    :pattern "{:posts {{:id 1} {:title (?t :when string?)}}}"}

   ;; --- Sequences: match vectors positionally ---
   {:name "Sequence [a b]"
    :description "Bind vector elements by position"
    :pattern "{:posts {{:id 1} {:tags [?first ?second]}}}"}

   {:name "Head + rest [a b*]"
    :description "First element + collect remaining with ?rest*"
    :pattern "{:posts {{:id 2} {:tags [?first ?rest*]}}}"}

   {:name "One or more [a+]"
    :description "Collect all elements — fails if vector is empty"
    :pattern "{:posts {{:id 1} {:tags [?t+]}}}"}

   {:name "Optional [a b?]"
    :description "Second tag is nil when absent (post 3 has one tag)"
    :pattern "{:posts {{:id 3} {:tags [?first ?second?]}}}"}

   ;; --- Composition: combine features ---
   {:name "Cross-collection"
    :description "Query users and posts in a single pattern"
    :pattern "{:users {{:id 1} {:name ?user}} :posts {{:id 1} {:title ?title}}}"}

   {:name "Mixed features"
    :description "Selective fields + sequence destructure in one lookup"
    :pattern "{:posts {{:id 2} {:title ?title :tags [?first ?rest*]}}}"}

   ;; --- Mutations: create, update, delete ---
   {:name "Create (nil key)"
    :description "nil key = create — auto-detected as mutation"
    :pattern "{:users {nil {:name \"Dave\" :email \"dave@example.com\" :role :user}}}"}

   {:name "Update fields"
    :description "Map value = update — click 'Read back' next to verify"
    :pattern "{:posts {{:id 1} {:title \"Hello Patterns!\"}}}"}

   {:name "Read back"
    :description "Read post 1 — title reflects the update above"
    :pattern "{:posts {{:id 1} {:title ?t :tags [?first ?second]}}}"}

   {:name "Delete (nil value)"
    :description "nil value = delete — use Reset in Data panel to restore"
    :pattern "{:users {{:id 2} nil}}"}])

(def syntax-reference
  [{:syntax "?x" :description "Bind value to x"}
   {:syntax "?_" :description "Wildcard (match any)"}
   {:syntax "?x?" :description "Optional (0-1)"}
   {:syntax "?x*" :description "Zero or more"}
   {:syntax "?x+" :description "One or more"}
   {:syntax "{}" :description "Map pattern"}
   {:syntax "[]" :description "Sequence pattern"}
   {:syntax ":when" :description "Predicate constraint"}
   {:syntax ":default" :description "Fallback value"}
   {:syntax "{nil data}" :description "Create (sandbox)"}
   {:syntax "{{:id 1} nil}" :description "Delete (sandbox)"}])
