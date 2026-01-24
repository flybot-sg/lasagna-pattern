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
   ;; => {:data {:user {:name \"Alice\"}} :vars {'n \"Alice\"}}

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
   Success:  `{:data {:user {:name \"Alice\"}} :vars {'n \"Alice\"}}`
   Failure:  `{:errors [{:code :schema-violation :reason \"...\"}]}`"
  (:require
   [sg.flybot.pullable.remote.http :as http]))

;;=============================================================================
;; Public API
;;=============================================================================

(def make-handler
  "Create a Ring handler for pull-based API.

   Arguments:
   - api-fn: Function (ring-request) → {:data lazy-map, :schema schema-map}

   Options:
   - :path - Base path for API (default \"/api\")

   Example:
   ```clojure
   (def handler
     (make-handler
       (fn [req]
         {:data (build-api (:session req))
          :schema (schema-for-role (:role req))})))
   ```"
  http/make-handler)

(defn wrap-api
  "Ring middleware that adds pull API at specified path.

   Delegates non-API requests to the wrapped handler.

   ```clojure
   (def app
     (-> my-handler
         (wrap-api my-api {:path \"/api/v1\"})))
   ```"
  ([handler api-fn]
   (wrap-api handler api-fn {}))
  ([next-handler api-fn {:keys [path] :or {path "/api"}}]
   (let [pull-handler (make-handler api-fn {:path path})
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

;; For custom collection types
(def Wireable
  "Protocol for types needing custom wire serialization.

   Implement this for custom collection types (like database-backed collections)
   that should be converted to standard Clojure data for Transit serialization.

   Example:
   ```clojure
   (extend-type MyCollection
     remote/Wireable
     (->wire [this] (vec this)))
   ```"
  http/Wireable)

(def ->wire
  "Convert a Wireable value to serializable Clojure data."
  http/->wire)

^:rct/test
(comment
  make-handler ;=>> fn?
  wrap-api ;=>> fn?
  encode ;=>> fn?
  decode) ;=>> fn?)
