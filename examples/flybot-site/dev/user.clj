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

(def ^:private dev-config
  "Dev mode config - auto-login as owner."
  {:dev {:mode? true :seed? true}
   :auth {:owner-emails "dev@localhost"}})

(defn start!
  "Start server with dev mode. Pass config to override."
  ([] (start! {}))
  ([config] (sys/start! (merge dev-config config))))

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
