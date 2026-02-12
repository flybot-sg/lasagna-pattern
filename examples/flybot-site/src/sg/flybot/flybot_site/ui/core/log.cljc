(ns sg.flybot.flybot-site.ui.core.log
  "Frontend logging mechanism - cross-platform for testability.")

;;=============================================================================
;; Log Levels
;;=============================================================================

(def levels
  "Log levels in order of severity."
  {:debug 0
   :info 1
   :warn 2
   :error 3
   :off 4})

(defonce ^:private config
  (atom {:level :debug
         :prefix "[blog]"}))

(defn set-level!
  "Set minimum log level. :debug :info :warn :error :off"
  [level]
  (swap! config assoc :level level))

(defn set-prefix!
  "Set log message prefix."
  [prefix]
  (swap! config assoc :prefix prefix))

;;=============================================================================
;; Core Logging
;;=============================================================================

(defn- should-log? [level]
  (let [min-level (:level @config)
        min-val (get levels min-level 0)
        level-val (get levels level 0)]
    (>= level-val min-val)))

(defn- format-msg [level args]
  (let [prefix (:prefix @config)
        level-str (str "[" (name level) "]")]
    (str prefix " " level-str " " (apply pr-str args))))

#?(:cljs
   (defn- log-to-console [level args]
     (when (should-log? level)
       (let [msg (format-msg level args)
             log-fn (case level
                      :debug js/console.debug
                      :info js/console.info
                      :warn js/console.warn
                      :error js/console.error
                      js/console.log)]
         (log-fn msg))))

   :clj
   (defn- log-to-console [level args]
     (when (should-log? level)
       (println (format-msg level args)))))

;;=============================================================================
;; Public API
;;=============================================================================

(defn debug
  "Log debug message."
  [& args]
  (log-to-console :debug args))

(defn info
  "Log info message."
  [& args]
  (log-to-console :info args))

(defn warn
  "Log warning message."
  [& args]
  (log-to-console :warn args))

(defn error
  "Log error message."
  [& args]
  (log-to-console :error args))

;;=============================================================================
;; Utility Functions
;;=============================================================================

(defn log-api-request
  "Log API request with pattern."
  [pattern]
  (debug "API request:" pattern))

(defn log-api-response
  "Log API response."
  [response]
  (debug "API response:" response))

(defn log-api-error
  "Log API error."
  [err pattern]
  (error "API error for" pattern ":" err))

(defn log-state-change
  "Log state transition."
  [action old-state new-state]
  (debug "State change:" action
         "view:" (:view old-state) "->" (:view new-state)))

(defn error->string
  "Convert error to string for classification."
  [err]
  (cond
    (instance? #?(:clj Throwable :cljs js/Error) err)
    #?(:clj  (.getMessage ^Throwable err)
       :cljs (.-message err))
    (string? err) err
    :else (str err)))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; error->string — string passthrough
  (error->string "network error")
  ;=> "network error"

  ;; error->string — other types
  (error->string 42)
  ;=> "42"

  ;; error->string — Throwable on JVM
  (error->string (ex-info "test error" {}))
  ;=> "test error"

  ;; Test level ordering
  (< (levels :debug) (levels :info) (levels :warn) (levels :error))
  ;=> true

  ;; Test should-log? respects level
  (do
    (set-level! :warn)
    [(should-log? :debug)
     (should-log? :info)
     (should-log? :warn)
     (should-log? :error)])
  ;=> [false false true true]

  ;; Test format-msg
  (do
    (set-level! :debug)
    (set-prefix! "[test]")
    (format-msg :info ["hello" {:a 1}]))
  ;=> "[test] [info] \"hello\" {:a 1}"

  ;; Reset for other tests
  (do
    (set-prefix! "[blog]")
    (set-level! :debug)
    nil))
  ;=> nil)
