(ns user
  "REPL development namespace.

   Quick Start:
     (start!)                        ; server with dev mode
     (def api (client/connect))      ; connect client
     (api '{:posts ?all})            ; pull data
     (sys/stop!)                     ; stop server"
  (:require
   [sg.flybot.flybot-site.server.system :as sys]
   [sg.flybot.pullable.remote.client :as client]))

(def dev-cfg
  "Development configuration - mirrors .env.example structure.
   Uses :dev mode for auto-login without OAuth.
   Explicitly sets backup-dir to nil to avoid loading from env var."
  {:mode :dev
   :server {:port 8080
            :base-url "http://localhost:8080"}
   :db {:backend :mem
        :id "blog"}
   :auth {:owner-emails "alice@flybot.sg"
          :allowed-email-pattern ".*@flybot\\.sg$"}
   :session {:timeout 43200}
   :init {:seed? true
          :backup-dir nil}  ; Explicitly nil to override any env var
   :uploads {:type :local
             :dir "resources/public/uploads"}
   ;; Dev user for auto-login (must match a seeded user for author filtering to work)
   :dev {:user {:id "sample-alice"
                :name "Alice Johnson"
                :email "alice@flybot.sg"}}})

(defn start!
  "Start server with dev config. Pass config to override."
  ([] (start! {}))
  ([config] (sys/start! (merge dev-cfg config))))

(comment
  ;; === START SERVER ===
  (start!)

  ;; === CONNECT CLIENT ===
  (def api (client/connect))

  ;; === QUERIES (role-as-top-level) ===
  (api '{:guest {:posts ?posts}})
  (api '{:guest {:posts {{:post/id 1} ?post}}})
  (client/schema api)

  ;; === STOP ===
  (sys/stop!))
