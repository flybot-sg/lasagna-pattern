(ns sg.flybot.flybot-site.auth
  "Role-based authentication for blog API.

   Two roles:
   - :owner - Full CRUD access, sees :me endpoint
   - :viewer - Read-only access (default)

   Owner is determined by email whitelist in config.

   ## Usage

   ```clojure
   (def api-fn (make-api {:owner-emails #{\"alice@example.com\"}}))

   ;; Returns role-appropriate {:data ... :schema ...}
   (api-fn {:session {:user-email \"alice@example.com\"}})  ; owner
   (api-fn {:session {:user-email \"bob@example.com\"}})    ; viewer
   (api-fn {})                                              ; viewer
   ```

   ## Integration

   Pair with any session-based auth (Google SSO, Auth0, etc.):
   1. Auth middleware sets :session {:user-email \"...\"}
   2. This module selects schema based on email"
  (:require
   [sg.flybot.flybot-site.api :as api]
   [sg.flybot.flybot-site.db :as db]))

;;=============================================================================
;; Role-Based API
;;=============================================================================

(defn owner?
  "Is email in the owner whitelist?"
  [config email]
  (contains? (:owner-emails config) email))

(def ^:private me-schema
  "Schema for the :me endpoint."
  {:email :string
   :name :string
   :picture :string
   :role :keyword})

(defn make-api
  "Create API function with role-based schema selection.

   Config:
   - :owner-emails - Set of owner email addresses
   - :conn - Datahike connection

   Reads session for user info to determine role:
   - Anonymous → viewer-schema (read-only)
   - Logged-in non-owner → viewer-schema + :me (read-only with identity)
   - Owner → full schema + :me (full CRUD access)"
  [{:keys [owner-emails conn] :or {owner-emails #{}}}]
  (fn [ring-request]
    (let [email (get-in ring-request [:session :user-email])
          name' (get-in ring-request [:session :user-name] email)
          picture (get-in ring-request [:session :user-picture])
          is-owner? (and email (owner? {:owner-emails owner-emails} email))
          base-api (api/make-api conn)
          me-data {:email email
                   :name name'
                   :picture picture
                   :role (if is-owner? :owner :viewer)}]
      (cond
        ;; Owner: full schema with :me
        is-owner?
        {:data (assoc base-api :me me-data)
         :schema (assoc api/schema :me me-schema)}

        ;; Logged-in non-owner: viewer-schema with :me
        email
        {:data (assoc base-api :me me-data)
         :schema (assoc api/viewer-schema :me me-schema)}

        ;; Anonymous: viewer-schema with :me (nil indicates not logged in)
        :else
        {:data (assoc base-api :me nil)
         :schema (assoc api/viewer-schema :me :any)}))))

^:rct/test
(comment
  ;; Setup
  (def conn (db/create-conn!))
  (db/seed! conn)
  (def api-fn (make-api {:owner-emails #{"owner@example.com"} :conn conn}))

  ;; Anonymous → viewer-schema with :me (nil indicates not logged in)
  (let [{:keys [data schema]} (api-fn {})]
    [(contains? schema :me)
     (nil? (:me data))
     (= (:posts schema) :any)])  ;; viewer-schema uses :any for posts
  ;=> [true true true]

  ;; Non-owner logged in → viewer-schema with :me
  (let [{:keys [data schema]} (api-fn {:session {:user-email "reader@example.com"
                                                 :user-name "Reader"
                                                 :user-picture "http://example.com/pic.jpg"}})]
    [(contains? schema :me)
     (:role (:me data))
     (= (:posts schema) :any)])  ;; viewer-schema uses :any for posts
  ;=> [true :viewer true]

  ;; Owner → full schema with :me
  (let [{:keys [data schema]} (api-fn {:session {:user-email "owner@example.com"
                                                 :user-name "Owner"
                                                 :user-picture "http://example.com/owner.jpg"}})]
    [(contains? schema :me)
     (:role (:me data))
     (:picture (:me data))
     (= (:posts schema) [:union [api/post-schema] {api/post-query api/post-schema}])])
  ;=> [true :owner "http://example.com/owner.jpg" true]

  ;; All users can read posts
  (count (seq (:posts (:data (api-fn {})))))
  ;=> 10

  ;; Cleanup
  (db/release-conn! conn))
