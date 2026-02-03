(ns user
  "REPL development namespace.

   Quick Start:
     (start!)                        ; server with dev mode
     (def api (client/connect))      ; connect client
     (api '{:posts ?all})            ; pull data
     (sys/stop!)                     ; stop server"
  (:require
   [sg.flybot.flybot-site.system :as sys]
   [sg.flybot.pullable.remote.client :as client]))

(def dev-cfg
  "Development configuration - mirrors .env.example structure.
   Uses :dev mode for auto-login without OAuth."
  {:mode :dev
   :server {:port 8080
            :base-url "http://localhost:8080"}
   :db {:backend :mem
        :id "blog"}
   :auth {:owner-emails "alice@flybot.sg"
          :allowed-email-pattern ".*@flybot\\.sg$"}
   :session {:timeout 43200}
   :init {:seed? true}
   :uploads {:type :local
             :dir "resources/public/uploads"}})

(defn start!
  "Start server with dev config. Pass config to override."
  ([] (start! {}))
  ([config] (sys/start! (merge dev-cfg config))))

(comment
  ;; === START SERVER ===
  (start!)

  ;; === CONNECT CLIENT ===
  (def api (client/connect))

  ;; === QUERIES ===
  (api '{:posts ?posts})
  (api '{:posts {{:post/id 1} ?post}})
  (client/schema api)

  ;; === STOP ===
  (sys/stop!))
