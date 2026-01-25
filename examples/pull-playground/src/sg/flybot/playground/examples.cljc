(ns sg.flybot.playground.examples
  "Pre-built example patterns for the playground.")

(def examples
  [{:name "Basic Binding"
    :description "Extract a value into a variable"
    :pattern "{:name ?name}"
    :data "{:name \"Alice\" :age 30}"}

   {:name "Multiple Bindings"
    :description "Extract multiple values at once"
    :pattern "{:name ?name :age ?age}"
    :data "{:name \"Bob\" :age 25 :city \"NYC\"}"}

   {:name "Nested Maps"
    :description "Match patterns in nested structures"
    :pattern "{:user {:name ?name :email ?email}}"
    :data "{:user {:name \"Carol\" :email \"carol@example.com\" :id 123}}"}

   {:name "Wildcard"
    :description "Match any value without binding"
    :pattern "{:status ?_ :data ?data}"
    :data "{:status \"ok\" :data {:items [1 2 3]}}"}

   {:name "Sequence Binding"
    :description "Extract elements from sequences"
    :pattern "[?first ?second ?rest*]"
    :data "[1 2 3 4 5]"}

   {:name "Predicate Constraint"
    :description "Match only if predicate succeeds"
    :pattern "{:age (?age :when pos?)}"
    :data "{:name \"Dave\" :age 42}"}

   {:name "Default Value"
    :description "Provide fallback for missing keys"
    :pattern "{:name ?name :nickname (?nick :default \"N/A\")}"
    :data "{:name \"Eve\"}"}

   {:name "Indexed Lookup"
    :description "Look up by key pattern (ILookup)"
    :pattern "{:users {{:id 2} ?user}}"
    :data "{:users [{:id 1 :name \"A\"} {:id 2 :name \"B\"} {:id 3 :name \"C\"}]}"}])

(def syntax-reference
  [{:syntax "?x" :description "Bind value to x"}
   {:syntax "?_" :description "Wildcard (match any)"}
   {:syntax "?x?" :description "Optional (0-1)"}
   {:syntax "?x*" :description "Zero or more"}
   {:syntax "?x+" :description "One or more"}
   {:syntax "{}" :description "Map pattern"}
   {:syntax "[]" :description "Sequence pattern"}
   {:syntax ":when" :description "Predicate constraint"}
   {:syntax ":default" :description "Fallback value"}])
