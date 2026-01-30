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
  "Dev defaults - uses BLOG_MODE from env, falls back to :dev if not set."
  (cond-> {:init {:seed? true}
           :auth {:owner-emails "dev@localhost"}}
    (nil? (System/getenv "BLOG_MODE")) (assoc :mode :dev)))

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
