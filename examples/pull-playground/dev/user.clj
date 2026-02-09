;; SCI + shadow-cljs workaround (babashka/sci#832):
;; Pre-load CLJS analyzer API so SCI's copy-var macro doesn't NPE.
(try (requiring-resolve 'cljs.analyzer.api/ns-resolve) (catch Exception _ nil))

(ns user
  "REPL helpers for playground backend development.

   Start the demo server:
     (start!)

   Stop:
     (stop!)"
  (:require [sg.flybot.playground.server.main :as server]))

(defn start!
  "Start the playground demo server on port 8081."
  []
  (server/start!))

(defn stop!
  "Stop the server."
  []
  (server/stop!))

(defn restart!
  "Restart the server."
  []
  (stop!)
  (start!))

(comment
  ;; Start the demo server
  (start!)

  ;; Stop the server
  (stop!)

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
