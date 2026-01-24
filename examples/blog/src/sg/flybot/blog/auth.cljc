(ns sg.flybot.blog.auth
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
   [sg.flybot.blog.api :as api]
   [sg.flybot.blog.db :as db]))

;;=============================================================================
;; Role-Based API
;;=============================================================================

(defn owner?
  "Is email in the owner whitelist?"
  [config email]
  (contains? (:owner-emails config) email))

(defn make-api
  "Create API function with role-based schema selection.

   Config:
   - :owner-emails - Set of owner email addresses
   - :conn - Datahike connection

   Reads :user-email from Ring session to determine role.
   Owner gets :me endpoint and full schema; viewer gets read-only."
  [{:keys [owner-emails conn] :or {owner-emails #{}}}]
  (fn [ring-request]
    (let [email (get-in ring-request [:session :user-email])
          owner? (and email (owner? {:owner-emails owner-emails} email))
          base-api (api/make-api conn)]
      {:data (if owner?
               (assoc base-api :me {:email email
                                    :name (get-in ring-request [:session :user-name] email)
                                    :role :owner})
               base-api)
       :schema (if owner?
                 (assoc api/schema :me {:email :string :name :string :role :keyword})
                 api/schema)})))

^:rct/test
(comment
  ;; Setup
  (def conn (db/create-conn!))
  (db/seed! conn)
  (def api-fn (make-api {:owner-emails #{"owner@example.com"} :conn conn}))

  ;; Anonymous → viewer (no :me)
  (let [{:keys [schema]} (api-fn {})]
    (contains? schema :me))
  ;=> false

  ;; Non-owner → viewer
  (let [{:keys [schema]} (api-fn {:session {:user-email "reader@example.com"}})]
    (contains? schema :me))
  ;=> false

  ;; Owner → gets :me
  (let [{:keys [data schema]} (api-fn {:session {:user-email "owner@example.com"}})]
    [(contains? schema :me)
     (:email (:me data))])
  ;=> [true "owner@example.com"]

  ;; All users can read posts
  (count (seq (:posts (:data (api-fn {})))))
  ;=> 3

  ;; Cleanup
  (db/release-conn! conn))
