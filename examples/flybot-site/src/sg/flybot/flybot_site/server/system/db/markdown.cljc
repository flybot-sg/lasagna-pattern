(ns sg.flybot.flybot-site.server.system.db.markdown
  "Markdown with YAML frontmatter parsing for post content.

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
              ;; Filter out nil/empty tags to prevent db errors
              (cond-> (:tags frontmatter)
                (update :tags #(->> % (remove nil?) (remove str/blank?) vec)))))))))

(defn extract-frontmatter
  "Extract properties from markdown frontmatter in post data.

   Parses :post/content markdown, converts frontmatter fields to post keys:
   - :author → :post/author
   - :tags → :post/tags

   Strips frontmatter from content, keeping only the body."
  [data]
  (if-let [content (:post/content data)]
    (let [parsed (parse content)]
      (cond-> data
        (:content parsed) (assoc :post/content (:content parsed))
        (:author parsed) (assoc :post/author (:author parsed))
        (:tags parsed) (assoc :post/tags (:tags parsed))))
    data))

^:rct/test
(comment
  (parse nil) ;=> {}
  (parse "") ;=> {}
  (parse "Hello world") ;=> {:content "Hello world"}
  (parse "---\nauthor: Alice\ntags:\n  - clojure\n  - test\n---\n\nHello world")
  ;=> {:content "Hello world" :author "Alice" :tags ["clojure" "test"]}

  ;; Empty tags are filtered out
  (parse "---\nauthor: dev\ntags:\n  - \n---\n\n")
  ;=> {:author "dev" :tags [] :content ""}

  ;; extract-frontmatter converts to post fields
  (extract-frontmatter {:post/title "Test" :post/content "---\nauthor: Alice\ntags:\n  - clj\n---\n\nHello"})
  ;=> {:post/title "Test", :post/content "Hello", :post/author "Alice", :post/tags ["clj"]}

  ;; No content, returns data unchanged
  (extract-frontmatter {:post/title "Test"})
  ;=> {:post/title "Test"}

  ;; Content without frontmatter
  (extract-frontmatter {:post/title "Test" :post/content "Hello world"}))
  ;=> {:post/title "Test", :post/content "Hello world"})
