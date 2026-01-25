(ns user
  "REPL helpers for playground development."
  (:require [sg.flybot.playground.server :as server]))

(defn start []
  (server/start!))

(defn stop []
  (server/stop!))

(defn restart []
  (stop)
  (start))

(comment
  ;; Start the demo server
  (start)

  ;; Stop the server
  (stop)

  ;; Test pattern matching locally
  (require '[sg.flybot.pullable.impl :as impl])

  (let [pattern '{:users ?users}
        matcher (impl/compile-pattern pattern)
        result (matcher (impl/vmr server/sample-data))]
    {:val (:val result)
     :vars (:vars result)})

  ;; Test with indexed lookup
  (let [pattern '{:users {{:id 2} ?user}}
        matcher (impl/compile-pattern pattern)
        result (matcher (impl/vmr server/sample-data))]
    (:vars result)))
