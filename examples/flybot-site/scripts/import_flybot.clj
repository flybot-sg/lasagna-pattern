#!/usr/bin/env bb
;; Script to clear database and import posts from flybot.sg

(ns import-flybot)

;; This script needs to be run in a Clojure REPL with the project deps loaded
;; Usage: clj -M:dev -e "(load-file \"scripts/import_flybot.clj\")"

(println "Import script loaded. Run (import-all!) from a REPL with project deps.")

(defn import-all! []
  (require '[sg.flybot.flybot-site.server.system.db :as db])
  (require '[sg.flybot.flybot-site.server.system.backup :as backup])
  (require '[datahike.api :as d])

  (let [cfg db/default-cfg
        ;; Create fresh database
        _ (println "Creating fresh database...")
        conn (db/create-conn! cfg)

        ;; Import from backups
        _ (println "Importing posts from backups...")
        result (backup/import-all! conn "./backups")]

    (println "Import complete:" result)
    (println "Total posts:" (count (d/q '[:find ?e :where [?e :post/id _]] @conn)))

    ;; Release connection
    (db/release-conn! conn)
    result))
