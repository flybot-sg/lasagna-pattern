(ns sg.flybot.pullable.remote
  "Pull-based remote protocol over HTTP.

   ## Server Quick Start

   ```clojure
   (require '[sg.flybot.pullable.remote :as remote])

   ;; Define API as a function: request → {:data ... :schema ...}
   (defn my-api [ring-request]
     {:data {:user {:name \"Alice\" :age 30}}
      :schema {:user {:name :string :age :number}}})

   ;; Create Ring handler
   (def handler (remote/make-handler my-api))
   ```

   ## Client Quick Start (JVM only)

   ```clojure
   (require '[sg.flybot.pullable.remote.client :as client])

   (def api (client/connect \"http://localhost:8080/api\"))
   (api '{:user {:name ?n}})
   ;; => {'n \"Alice\"}

   (client/schema api)  ; introspect
   ```

   ## Endpoints

   - `POST /api` - Execute pull pattern
   - `GET /api/_schema` - Schema introspection (session-aware)

   ## Wire Format

   Content negotiation via Accept/Content-Type headers:
   - `application/transit+json` (default)
   - `application/transit+msgpack`
   - `application/edn`

   ## Request/Response

   Request:  `{:pattern '{:user {:name ?n}}}`
   Success:  `{'n \"Alice\"}`
   Failure:  `{:errors [{:code :schema-violation :reason \"...\"}]}`"
  (:require
   [sg.flybot.pullable.remote.http :as http]))

;;=============================================================================
;; Public API
;;=============================================================================

(def make-handler
  "Create a Ring handler for pull-based API.

   Arguments:
   - api-fn: Function (ring-request) → {:data lazy-map, :schema schema-map,
                                        :writes write-schemas, :errors errors-config}

   The api-fn returns :errors config for error handling:
   - :detect - keyword or fn to detect errors in mutation results
   - :codes  - Map of error-type to HTTP status

   Collections return errors as data: {:error {:type :forbidden :message \"...\"}}

   Options:
   - :path      - Base path for API (default \"/api\")
   - :ex->error - (fn [throwable {:pattern p :context ring-request}]) →
                  {:code _ :reason _}; default `default-ex->error`;
                  must not throw.

   Example:
   ```clojure
   (def handler
     (make-handler
       (fn [req]
         {:data (build-api (:session req))
          :schema my-schema
          :errors {:detect :error
                   :codes {:forbidden 403}}})))
   ```"
  http/make-handler)

(defn wrap-api
  "Ring middleware that adds pull API at specified path.

   Delegates non-API requests to the wrapped handler.

   Options:
   - :path      - Base path for API (default \"/api\")
   - :ex->error - Exception → wire-error conversion, see `make-handler`

   ```clojure
   (def app
     (-> my-handler
         (wrap-api my-api {:path \"/api/v1\"})))
   ```"
  ([handler api-fn]
   (wrap-api handler api-fn {}))
  ([next-handler api-fn {:keys [path] :or {path "/api"} :as opts}]
   (let [pull-handler (make-handler api-fn opts)
         schema-path (str path "/_schema")]
     (fn [request]
       (if (or (= (:uri request) path)
               (= (:uri request) schema-path))
         (pull-handler request)
         (next-handler request))))))

(def parse-mutation
  "Detect if pattern is a mutation. Returns {:path :query :value} or nil.

   Mutations use nil (create) or map (update/delete) as query keys.
   Read patterns (keyword query keys or ?-variable values) return nil."
  http/parse-mutation)

(def execute
  "Execute a pull pattern directly (no HTTP). Used by in-process callers
   like browser sandboxes that share the same execution engine as the server.

   api-fn:  (fn [context] {:data ... :schema ... :writes ... :errors ...})
   pattern: Clojure data structure (EDN)
   opts:    {:params    {...}  ; $-param substitution
             :resolve   fn     ; symbol resolver (default: safe whitelist)
             :eval-fn   fn     ; form evaluator (default: blocked)
             :context   map    ; passed to api-fn
             :ex->error fn}    ; (fn [throwable {:pattern p :context ctx}]) →
                               ; {:code _ :reason _}; default
                               ; `default-ex->error`; must not throw

   Returns vars map on success, {:errors [...]} on failure.

   ```clojure
   (def api-fn
     (fn [_ctx] {:data {:posts posts-coll}}))

   (execute api-fn '{:posts ?all})
   ;; => {'all [...]}
   ```"
  http/execute)

(def default-ex->error
  "Exception → {:code :execution-error :reason <message>}.
   Default for :ex->error; compose with it to log then delegate."
  http/default-ex->error)

;; For client implementations
(def encode
  "Encode Clojure data to bytes. Format: :transit-json, :transit-msgpack, :edn."
  http/encode)

(def decode
  "Decode bytes to Clojure data."
  http/decode)

^:rct/test
(comment
  make-handler ;=>> fn?
  wrap-api ;=>> fn?
  parse-mutation ;=>> fn?
  execute ;=>> fn?
  default-ex->error ;=>> fn?
  encode ;=>> fn?
  decode ;=>> fn?
  )
