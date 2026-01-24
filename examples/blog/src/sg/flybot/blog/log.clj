(ns sg.flybot.blog.log
  "Backend logging mechanism using Timbre (already included via Datahike)."
  (:require [taoensso.timbre :as timbre]))

;;=============================================================================
;; Configuration
;;=============================================================================

(def ^:private default-config
  {:min-level :debug
   :ns-filter {:allow #{"sg.flybot.blog.*"}}})

(defn configure!
  "Configure logging. Options:
   - :level - :trace :debug :info :warn :error :fatal (default :debug)
   - :pattern - Output pattern (default: timestamp + level + ns + message)"
  ([] (configure! {}))
  ([{:keys [level] :or {level :debug}}]
   (timbre/merge-config!
    {:min-level level
     :output-fn (fn [{:keys [level ?ns-str msg_]}]
                  (str "[blog] [" (name level) "] "
                       (when ?ns-str (str "[" (last (clojure.string/split ?ns-str #"\.")) "] "))
                       (force msg_)))})))

(defn set-level!
  "Set minimum log level: :trace :debug :info :warn :error :fatal"
  [level]
  (timbre/set-min-level! level))

;;=============================================================================
;; Core Logging (delegate to Timbre)
;;=============================================================================

(defmacro debug [& args] `(timbre/debug ~@args))
(defmacro info [& args] `(timbre/info ~@args))
(defmacro warn [& args] `(timbre/warn ~@args))
(defmacro error [& args] `(timbre/error ~@args))

;;=============================================================================
;; Domain-Specific Logging
;;=============================================================================

;; Server/HTTP
(defn log-request
  "Log incoming HTTP request."
  [req]
  (timbre/debug "HTTP" (:request-method req) (:uri req)
                (when-let [ct (get-in req [:headers "content-type"])]
                  (str "content-type=" ct))))

(defn log-response
  "Log HTTP response."
  [status uri ms]
  (let [level (cond
                (>= status 500) :error
                (>= status 400) :warn
                :else :debug)]
    (timbre/log level "HTTP response" status uri (str ms "ms"))))

;; API/Pattern
(defn log-api-request
  "Log API pull request."
  [pattern]
  (timbre/debug "API request:" (pr-str pattern)))

(defn log-api-response
  "Log API response."
  [response]
  (timbre/debug "API response:" (pr-str response)))

(defn log-api-error
  "Log API error."
  [err pattern]
  (timbre/error "API error for" (pr-str pattern) ":" err))

;; Database
(defn log-db-op
  "Log database operation."
  [op entity-type id]
  (timbre/debug "DB" op entity-type (when id (str "id=" id))))

(defn log-db-create
  "Log entity creation."
  [entity-type result]
  (timbre/info "DB created" entity-type "id=" (:post/id result)))

(defn log-db-update
  "Log entity update."
  [entity-type id]
  (timbre/info "DB updated" entity-type "id=" id))

(defn log-db-delete
  "Log entity deletion."
  [entity-type id]
  (timbre/info "DB deleted" entity-type "id=" id))

;; System lifecycle
(defn log-startup
  "Log server startup."
  [port]
  (timbre/info "Server starting on port" port))

(defn log-shutdown
  "Log server shutdown."
  []
  (timbre/info "Server shutting down"))

(defn log-db-connected
  "Log database connection."
  [cfg]
  (timbre/info "Database connected:" (:store cfg)))

(defn log-db-seeded
  "Log database seeding."
  [count]
  (timbre/info "Database seeded with" count "posts"))

;;=============================================================================
;; Request Timing Middleware
;;=============================================================================

(defn wrap-request-logging
  "Ring middleware that logs requests and response times."
  [handler]
  (fn [req]
    (let [start (System/currentTimeMillis)
          _ (log-request req)
          resp (handler req)
          ms (- (System/currentTimeMillis) start)]
      (log-response (:status resp) (:uri req) ms)
      resp)))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; Verify macros compile
  (with-out-str (debug "test debug"))
  ;=>> string?

  ;; Verify domain loggers don't throw
  (log-db-op :fetch :post 1)
  ;=> nil

  ;; Verify middleware wraps correctly
  (let [handler (wrap-request-logging (fn [_] {:status 200}))
        resp (handler {:request-method :get :uri "/test"})]
    (:status resp)))
  ;=> 200)
