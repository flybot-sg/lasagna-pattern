;; SCI + shadow-cljs workaround (babashka/sci#832):
;; Pre-load CLJS analyzer API so SCI's copy-var macro doesn't NPE.
(try (requiring-resolve 'cljs.analyzer.api/ns-resolve) (catch Exception _ nil))

(ns user
  "REPL helpers for playground development.

   Start:   (start!)
   Stop:    (stop!)
   Restart: (restart!)"
  (:require [sg.flybot.playground.server.main :as server]))

(defn start!  [] (server/start!))
(defn stop!   [] (server/stop!))
(defn restart! [] (stop!) (start!))

(comment
  (start!)
  (stop!)

  ;; Test pattern matching against running server data
  (require '[sg.flybot.pullable.impl :as impl])

  (let [pattern '{:users ?users}
        data    (:data @server/system)
        result  ((impl/compile-pattern pattern) (impl/vmr data))]
    (:vars result))

  (let [pattern '{:users {{:id 2} ?user}}
        data    (:data @server/system)
        result  ((impl/compile-pattern pattern) (impl/vmr data))]
    (:vars result)))
