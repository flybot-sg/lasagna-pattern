(ns sg.flybot.blog.markdown
  "Markdown with YAML frontmatter parsing.

   Posts are stored as markdown with YAML frontmatter:

   ```markdown
   ---
   author: Alice
   tags:
     - clojure
     - patterns
   ---

   The actual post content in markdown...
   ```

   Frontmatter fields become post properties."
  (:require
   [clojure.string :as str]
   #?(:clj [clj-yaml.core :as yaml])))

(defn parse
  "Parse markdown with YAML frontmatter into a map.

   Returns {:content \"...\" :author \"...\" :tags [...] ...}
   All frontmatter fields are merged into the result."
  [markdown]
  (if (or (nil? markdown) (str/blank? markdown))
    {}
    (let [lines (str/split-lines markdown)]
      (if (not= (first lines) "---")
        {:content markdown}
        (let [end-idx (->> (rest lines)
                           (take-while #(not= % "---"))
                           count
                           inc)
              frontmatter-str (->> (subvec (vec lines) 1 end-idx)
                                   (str/join "\n"))
              content (->> (subvec (vec lines) (+ end-idx 1))
                           (str/join "\n")
                           str/trim)
              #?@(:clj [frontmatter (yaml/parse-string frontmatter-str :keywords true)]
                  :cljs [frontmatter {}])]
          (-> frontmatter
              (assoc :content content)
              (cond-> (:tags frontmatter) (update :tags vec))))))))

^:rct/test
(comment
  (parse nil) ;=> {}
  (parse "") ;=> {}
  (parse "Hello world") ;=> {:content "Hello world"}
  (parse "---\nauthor: Alice\ntags:\n  - clojure\n  - test\n---\n\nHello world"))
  ;=> {:content "Hello world" :author "Alice" :tags ["clojure" "test"]})
