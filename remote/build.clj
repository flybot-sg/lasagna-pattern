(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.edn :as edn]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'sg.flybot/lasagna-remote)
(def version (-> (edn/read-string (slurp "resources/version.edn")) :version))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(def pom-data
  [[:description "HTTP transport for pull-based pattern APIs"]
   [:url "https://github.com/flybot-sg/lasagna-pattern"]
   [:licenses
    [:license
     [:name "The Unlicense"]
     [:url "https://unlicense.org/"]]]
   [:developers
    [:developer
     [:name "Robert Luo"]]
    [:developer
     [:name "Loic Blanchard"]]]
   [:scm
    [:url "https://github.com/flybot-sg/lasagna-pattern"]
    [:connection "scm:git:https://github.com/flybot-sg/lasagna-pattern.git"]
    [:developerConnection "scm:git:ssh://git@github.com/flybot-sg/lasagna-pattern.git"]
    [:tag (str "remote-v" version)]]])

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :pom-data pom-data})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn install [_]
  (jar nil)
  (b/install {:basis @basis
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir}))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact (b/resolve-path jar-file)
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
