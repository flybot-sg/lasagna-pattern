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
   - api-fn: Function (ring-request) → {:data lazy-map, :schema schema-map, :errors errors-config}

   The api-fn returns :errors config for error handling:
   - :detect - keyword or fn to detect errors in mutation results
   - :codes  - Map of error-type to HTTP status

   Collections return errors as data: {:error {:type :forbidden :message \"...\"}}

   Options:
   - :path - Base path for API (default \"/api\")

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
   - :path - Base path for API (default \"/api\")

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
  encode ;=>> fn?
  decode ;=>> fn?
  )
