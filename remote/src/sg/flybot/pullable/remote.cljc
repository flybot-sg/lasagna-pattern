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

   ## CRUD Collections

   For data sources supporting CRUD operations:

   ```clojure
   ;; 1. Implement DataSource for your backend
   (defrecord MyDB [db-atom]
     remote/DataSource
     (fetch [_ query] (get @db-atom (:id query)))
     (list-all [_] (vals @db-atom))
     (create! [_ data] ...)
     (update! [_ query data] ...)
     (delete! [_ query] ...))

   ;; 2. Create collection
   (def items (remote/collection (->MyDB db)))

   ;; 3. Use via ILookup/Seqable/Mutable
   (get items {:id 3})           ; fetch
   (seq items)                   ; list
   (remote/mutate! items nil data)    ; create
   (remote/mutate! items {:id 3} data) ; update
   (remote/mutate! items {:id 3} nil)  ; delete
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
   [sg.flybot.pullable.remote.http :as http]
   [sg.flybot.pullable.remote.collection :as coll]))

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

;;=============================================================================
;; CRUD Collections
;;=============================================================================

(def DataSource
  "Protocol for CRUD data source backends.

   Methods:
   - (fetch [this query]) - Fetch item matching query, returns item or nil
   - (list-all [this]) - List all items, returns sequence
   - (create! [this data]) - Create item, returns created item
   - (update! [this query data]) - Update item, returns updated item or nil
   - (delete! [this query]) - Delete item, returns true/false"
  coll/DataSource)

(def fetch
  "Fetch item from DataSource matching query."
  coll/fetch)

(def list-all
  "List all items from DataSource."
  coll/list-all)

(def create!
  "Create item in DataSource. Returns created item."
  coll/create!)

(def update!
  "Update item in DataSource matching query. Returns updated item or nil."
  coll/update!)

(def delete!
  "Delete item from DataSource matching query. Returns true/false."
  coll/delete!)

(def Mutable
  "Protocol for collections supporting mutations.

   Use mutate! for CRUD:
   - (mutate! coll nil data) -> CREATE
   - (mutate! coll query data) -> UPDATE
   - (mutate! coll query nil) -> DELETE"
  coll/Mutable)

(def mutate!
  "Perform mutation on a Mutable collection.

   - (mutate! coll nil data) -> CREATE: returns created item
   - (mutate! coll query data) -> UPDATE: returns updated item
   - (mutate! coll query nil) -> DELETE: returns true/false"
  coll/mutate!)

(def collection
  "Create a Collection wrapping a DataSource.

   Options:
   - :indexes - Set of indexed field sets (default #{#{:id}})

   Returns collection implementing ILookup, Seqable, Counted, Mutable, Wireable.

   Example:
   ```clojure
   (def items (collection my-datasource {:indexes #{#{:id} #{:email}}}))
   (get items {:id 3})      ; fetch by id
   (get items {:email \"x\"}) ; fetch by email (indexed)
   (seq items)              ; list all
   (mutate! items nil data) ; create
   ```"
  coll/collection)

;; For custom wire serialization (advanced use)
(def Wireable
  "Protocol for types needing custom wire serialization.

   Implement this for custom types that should be converted to
   standard Clojure data for Transit serialization.

   Note: Collection already implements Wireable, so you typically
   don't need this unless creating custom types."
  http/Wireable)

(def ->wire
  "Convert a Wireable value to serializable Clojure data."
  http/->wire)

^:rct/test
(comment
  make-handler ;=>> fn?
  wrap-api ;=>> fn?
  encode ;=>> fn?
  decode ;=>> fn?
  collection ;=>> fn?
  mutate!) ;=>> fn?)
