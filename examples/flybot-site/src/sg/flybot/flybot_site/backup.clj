(ns sg.flybot.flybot-site.backup
  "Post backup and restore.

   Export posts to markdown files, import from a directory.

   ## Usage

   ```clojure
   (export-all! conn \"./backups\")
   (import-all! conn \"./backups\")
   ```"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clj-yaml.core :as yaml]
   [datahike.api :as d]
   [sg.flybot.pullable.collection :as coll]
   [sg.flybot.flybot-site.db :as db]
   [sg.flybot.flybot-site.log :as log])
  (:import
   [java.time Instant]
   [java.time.format DateTimeFormatter]))

;;=============================================================================
;; Date Helpers
;;=============================================================================

(defn- date->iso [^java.util.Date d]
  (when d (.format DateTimeFormatter/ISO_INSTANT (.toInstant d))))

(defn- iso->date [s]
  (when s (java.util.Date/from (Instant/parse s))))

;;=============================================================================
;; Export
;;=============================================================================

(defn- post->markdown
  "Convert post to markdown with full metadata in frontmatter."
  [{:post/keys [id title author tags created-at updated-at content]}]
  (let [;; Strip existing frontmatter from content body
        body (if (and content (str/starts-with? content "---"))
               (if-let [match (re-find #"(?s)^---\n.*?\n---\n?" content)]
                 (str/trim (subs content (count match)))
                 content)
               (or content ""))
        meta (cond-> {:title title}
               id (assoc :id id)
               author (assoc :author author)
               (seq tags) (assoc :tags (vec tags))
               created-at (assoc :created-at (date->iso created-at))
               updated-at (assoc :updated-at (date->iso updated-at)))]
    (str "---\n"
         (yaml/generate-string meta :dumper-options {:flow-style :block})
         "---\n\n"
         body)))

(defn export-all!
  "Export all posts to directory as .md files.
   Returns {:count n :dir path}."
  [conn dir]
  (log/info "Exporting posts to:" dir)
  (let [posts (coll/list-all (db/->PostsDataSource conn))
        dir-file (io/file dir)]
    (.mkdirs dir-file)
    (doseq [{:post/keys [id title] :as post} posts
            :let [filename (str id "-" (str/replace (or title "untitled") #"[^a-zA-Z0-9]+" "-") ".md")
                  file (io/file dir filename)]]
      (spit file (post->markdown post)))
    (log/info "Exported" (count posts) "posts")
    {:count (count posts) :dir (.getAbsolutePath dir-file)}))

;;=============================================================================
;; Import
;;=============================================================================

(defn- parse-backup
  "Parse backup file into post entity."
  [content]
  (when (str/starts-with? content "---")
    (let [lines (str/split-lines content)
          end-idx (->> (rest lines) (take-while #(not= % "---")) count inc)
          yaml-str (str/join "\n" (subvec (vec lines) 1 end-idx))
          body (str/trim (str/join "\n" (subvec (vec lines) (inc end-idx))))
          {:keys [id title author tags created-at updated-at]} (yaml/parse-string yaml-str :keywords true)]
      {:post/id (long id)
       :post/title title
       :post/author author
       :post/tags (set tags)
       :post/created-at (iso->date created-at)
       :post/updated-at (iso->date updated-at)
       :post/content (str "---\n"
                          (when author (str "author: " author "\n"))
                          (when (seq tags)
                            (str "tags:\n" (str/join "\n" (map #(str "  - " %) tags)) "\n"))
                          "---\n\n" body)})))

(defn import-all!
  "Import all .md files from directory.
   Preserves IDs and timestamps. Returns {:count n :dir path}."
  [conn dir]
  (log/info "Importing posts from:" dir)
  (let [files (->> (.listFiles (io/file dir))
                   (filter #(str/ends-with? (.getName %) ".md")))
        entities (keep #(parse-backup (slurp %)) files)]
    (when (seq entities)
      (d/transact conn (vec entities)))
    (log/info "Imported" (count entities) "posts")
    {:count (count entities) :dir (.getAbsolutePath (io/file dir))}))

^:rct/test
(comment
  (def conn (db/create-conn!))
  (db/seed! conn)

  ;; Export
  (def result (export-all! conn "/tmp/flybot-backup-test"))
  (:count result) ;=> 3

  ;; Check file content
  (str/includes? (slurp "/tmp/flybot-backup-test/1-Welcome-to-My-Blog.md") "title: Welcome") ;=> true

  ;; Import to fresh db
  (def conn2 (db/create-conn! {:store {:backend :mem :id "test2"}
                               :schema-flexibility :write
                               :keep-history? true}))
  (:count (import-all! conn2 "/tmp/flybot-backup-test")) ;=> 3

  ;; Cleanup
  (db/release-conn! conn)
  (db/release-conn! conn2))
