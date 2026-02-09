(ns sg.flybot.playground.common.data
  "Sample data and schema shared between sandbox and remote modes.")

(def default-data
  {:users [{:id 1 :name "Alice" :email "alice@example.com" :role :admin}
           {:id 2 :name "Bob" :email "bob@example.com" :role :user}
           {:id 3 :name "Carol" :email "carol@example.com" :role :user}]
   :posts [{:id 1 :title "Hello World" :author "Alice" :tags ["intro" "welcome"]}
           {:id 2 :title "Pattern Matching" :author "Bob" :tags ["tutorial" "patterns"]}
           {:id 3 :title "Advanced Topics" :author "Alice" :tags ["advanced"]}]
   :config {:version "1.0.0"
            :features {:dark-mode true :notifications false}}})

(def default-schema
  "Malli hiccup form. Server wraps in m/schema, sandbox uses raw."
  [:map {:version "1.0.0"
         :doc "Sample API for Pull Pattern Playground"}
   [:users {:doc "User accounts"}
    [:vector {:ilookup true}
     [:map
      [:id {:doc "Unique identifier" :example 1} :int]
      [:name {:doc "Display name" :example "Alice"} :string]
      [:email {:doc "Email address" :example "alice@example.com"} :string]
      [:role {:doc "User role" :example :admin} :keyword]]]]
   [:posts {:doc "Blog posts"}
    [:vector {:ilookup true}
     [:map
      [:id {:doc "Post identifier" :example 1} :int]
      [:title {:doc "Post title" :example "Hello World"} :string]
      [:author {:doc "Author name" :example "Alice"} :string]
      [:tags {:doc "Post tags" :example ["intro" "welcome"]} [:vector :string]]]]]
   [:config {:doc "Application configuration"}
    [:map
     [:version {:doc "App version"} :string]
     [:features {:doc "Feature flags"}
      [:map
       [:dark-mode {:doc "Dark mode enabled"} :boolean]
       [:notifications {:doc "Notifications enabled"} :boolean]]]
     [:debug {:optional true :doc "Debug mode"} :boolean]]]])
