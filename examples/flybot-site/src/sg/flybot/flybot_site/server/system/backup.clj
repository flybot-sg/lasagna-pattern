(ns sg.flybot.flybot-site.server.system.backup
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
   [sg.flybot.flybot-site.server.system.db :as db]
   [com.brunobonacci.mulog :as mu])
  (:import
   [java.time Instant]
   [java.time.format DateTimeFormatter]))

;;=============================================================================
;; Date Helpers
;;=============================================================================

(defn- date->iso [^java.util.Date d]
  (when d (.format DateTimeFormatter/ISO_INSTANT (.toInstant d))))

(defn- iso->date [s]
  (cond
    (nil? s) nil
    (instance? java.util.Date s) s
    :else (java.util.Date/from (Instant/parse s))))

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
  (mu/log ::export-start :dir dir)
  (let [posts (coll/list-all (db/->PostsDataSource conn))
        dir-file (io/file dir)]
    (.mkdirs dir-file)
    (doseq [{:post/keys [id title] :as post} posts
            :let [filename (str id "-" (str/replace (or title "untitled") #"[^a-zA-Z0-9]+" "-") ".md")
                  file (io/file dir filename)]]
      (spit file (post->markdown post)))
    (mu/log ::export-complete :count (count posts) :dir (.getAbsolutePath dir-file))
    {:count (count posts) :dir (.getAbsolutePath dir-file)}))

;;=============================================================================
;; Import
;;=============================================================================

(defn- ensure-placeholder-users!
  "Create placeholder users for all unique authors in entities.
   Placeholder users have user/id starting with 'placeholder:'.
   Returns map of author-name -> user-id."
  [conn entities]
  (let [authors (->> entities (keep :post/author) (filter string?) distinct)]
    (into {}
          (for [author-name authors
                :let [placeholder-id (str "placeholder:" (random-uuid))
                      slug (db/slugify author-name)]]
            (do
              (when-not (db/get-user-by-name conn author-name)
                (d/transact conn [{:user/id placeholder-id
                                   :user/slug slug
                                   :user/name author-name
                                   :user/email ""
                                   :user/picture ""}])
                (mu/log ::placeholder-created :author author-name :slug slug))
              [author-name placeholder-id])))))

(defn- parse-backup
  "Parse backup file into post entity."
  [content]
  (when (str/starts-with? content "---")
    (let [lines (str/split-lines content)
          end-idx (->> (rest lines) (take-while #(not= % "---")) count inc)
          yaml-str (str/join "\n" (subvec (vec lines) 1 end-idx))
          body (str/trim (str/join "\n" (subvec (vec lines) (inc end-idx))))
          {:keys [id title author tags created-at updated-at featured?]} (yaml/parse-string yaml-str :keywords true)]
      (cond-> {:post/id (long id)
               :post/title title
               :post/author author
               :post/tags (set tags)
               :post/created-at (iso->date created-at)
               :post/updated-at (iso->date updated-at)
               :post/content (str "---\n"
                                  (when author (str "author: " author "\n"))
                                  (when (seq tags)
                                    (str "tags:\n" (str/join "\n" (map #(str "  - " %) tags)) "\n"))
                                  "---\n\n" body)}
        featured? (assoc :post/featured? true)))))

(defn import-all!
  "Import all .md files from directory.
   Creates placeholder users for authors and links posts to them.
   Preserves IDs and timestamps. Returns {:count n :dir path :users-created m}."
  [conn dir]
  (mu/log ::import-start :dir dir)
  (let [files (->> (.listFiles (io/file dir))
                   (filter #(str/ends-with? (.getName %) ".md")))
        entities (keep #(parse-backup (slurp %)) files)
        ;; Create placeholder users first
        author->user-id (ensure-placeholder-users! conn entities)
        ;; Convert author strings to user-id lookup refs
        entities-with-refs (for [e entities]
                             (if-let [author (:post/author e)]
                               (if-let [user-id (get author->user-id author)]
                                 (assoc e :post/author [:user/id user-id])
                                 e)
                               e))]
    (when (seq entities-with-refs)
      (d/transact conn (vec entities-with-refs)))
    (mu/log ::import-complete :count (count entities) :users (count author->user-id))
    {:count (count entities)
     :dir (.getAbsolutePath (io/file dir))
     :users-created (count author->user-id)}))

^:rct/test
(comment
  (def conn (db/create-conn!))
  (db/seed! conn)

  ;; Clean up previous test files
  (let [dir (io/file "/tmp/flybot-backup-test")]
    (when (.exists dir)
      (doseq [f (.listFiles dir)] (.delete f))))

  ;; Export
  (def result (export-all! conn "/tmp/flybot-backup-test"))
  (:count result) ;=> 11

  ;; Check file content
  (str/includes? (slurp "/tmp/flybot-backup-test/1-Welcome-to-Flybot.md") "title: Welcome") ;=> true

  ;; Import to fresh db
  (def conn2 (db/create-conn! {:store {:backend :mem :id "test2"}
                               :schema-flexibility :write
                               :keep-history? true}))
  (:count (import-all! conn2 "/tmp/flybot-backup-test")) ;=> 11

  ;; Cleanup
  (db/release-conn! conn)
  (db/release-conn! conn2))

^:rct/test
(comment
  ;; === Import with placeholder users ===
  (def conn (db/create-conn!))

  ;; Create a test backup file with Chinese author (using generic test name)
  (let [dir (io/file "/tmp/flybot-import-placeholder-test")]
    (.mkdirs dir)
    (spit (io/file dir "1-test.md")
          "---\nid: 1\ntitle: Test Post\nauthor: 张伟\ntags:\n  - test\ncreated-at: 2024-01-01T00:00:00Z\nupdated-at: 2024-01-01T00:00:00Z\n---\n\nContent here"))

  ;; Import should create placeholder user
  (def result (import-all! conn "/tmp/flybot-import-placeholder-test"))
  (:count result) ;=> 1
  (:users-created result) ;=> 1

  ;; Verify placeholder user was created (detected by ID prefix)
  (str/starts-with? (:user/id (db/get-user-by-name conn "张伟")) "placeholder:") ;=> true
  (:user/slug (db/get-user-by-name conn "张伟")) ;=> "zhangwei"

  ;; Verify post links to placeholder user
  (def p (db/posts conn))
  (:user/name (:post/author (get p {:post/id 1}))) ;=> "张伟"

  ;; Now simulate Google login - should claim the placeholder
  (db/upsert-user! conn #:user{:id "google-zhang" :email "zhang@flybot.sg" :name "张伟" :picture ""})
  (:user/id (:post/author (get p {:post/id 1}))) ;=> "google-zhang"
  (str/starts-with? (:user/id (db/get-user conn "google-zhang")) "placeholder:") ;=> false

  ;; Cleanup
  (db/release-conn! conn))
