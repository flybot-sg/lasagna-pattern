(ns sg.flybot.playground.ui.core.views.examples
  "Pre-built example patterns for the playground.")

(def examples
  [{:name "List all users"
    :description "Read all users from the collection"
    :pattern "{:users ?all}"}

   {:name "List all posts"
    :description "Read all posts from the collection"
    :pattern "{:posts ?all}"}

   {:name "Multiple bindings"
    :description "Extract multiple top-level keys at once"
    :pattern "{:users ?u :posts ?p}"}

   {:name "Lookup user by ID"
    :description "Find a specific user using indexed lookup"
    :pattern "{:users {{:id 2} ?user}}"}

   {:name "Read config"
    :description "Read the application configuration"
    :pattern "{:config ?cfg}"}

   {:name "Nested map"
    :description "Match nested structure inside config"
    :pattern "{:config {:features ?features}}"}

   {:name "Default value"
    :description "Provide fallback for missing keys"
    :pattern "{:config {:debug (?d :default false)}}"}

   {:name "Constrained lookup"
    :description "Find a post and apply a predicate constraint"
    :pattern "{:posts {{:id 1} {:title (?t :when string?)}}}"}

   {:name "Create user"
    :description "Create with nil key — auto-detected as mutation"
    :pattern "{:users {nil {:name \"Dave\" :email \"dave@example.com\" :role :user}}}"}

   {:name "Update user"
    :description "Update with map value — auto-detected as mutation"
    :pattern "{:users {{:id 1} {:name \"Alice Updated\"}}}"}

   {:name "Delete user"
    :description "Delete with nil value — auto-detected as mutation"
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

^:rct/test
(comment
  (count examples) ;=> 11
  (:name (first examples))) ;=> "List all users")
