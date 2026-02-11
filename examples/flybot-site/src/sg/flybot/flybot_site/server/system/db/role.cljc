(ns sg.flybot.flybot-site.server.system.db.role
  "Role CRUD operations and UserRolesDataSource.

   Manages user role grants and revocations. Roles are component entities
   owned by users, each with a name keyword and grant timestamp."
  (:require
   [datahike.api :as d]
   [sg.flybot.pullable.collection :as coll]
   [com.brunobonacci.mulog :as mu]))

;;=============================================================================
;; Role Queries
;;=============================================================================

(defn get-user-roles
  "Get set of role keywords for user. Returns empty set if no roles."
  [conn user-id]
  (when user-id
    (->> (d/q '[:find [?role-name ...]
                :in $ ?uid
                :where [?u :user/id ?uid]
                [?u :user/roles ?r]
                [?r :role/name ?role-name]]
              @conn user-id)
         set)))

(defn get-user-roles-detailed
  "Get roles with grant timestamps for user. Returns vector of {:role/name :role/granted-at}."
  [conn user-id]
  (when user-id
    (->> (d/q '[:find [(pull ?r [:role/name :role/granted-at]) ...]
                :in $ ?uid
                :where [?u :user/id ?uid]
                [?u :user/roles ?r]]
              @conn user-id)
         (sort-by :role/granted-at))))

;;=============================================================================
;; Role Mutations
;;=============================================================================

(defn grant-role!
  "Add a role to user with grant timestamp.
   Uses explicit :db/add to ensure accumulative behavior (not replace).
   No-op if user already has the role."
  [conn user-id role]
  (when (and user-id role)
    (let [existing-roles (get-user-roles conn user-id)]
      (when-not (contains? existing-roles role)
        (let [temp-id (str "role-" (random-uuid))]
          (d/transact conn [{:db/id temp-id
                             :role/name role
                             :role/granted-at (java.util.Date.)}
                            [:db/add [:user/id user-id] :user/roles temp-id]])
          (mu/log ::role-granted :user-id user-id :role role))))))

(defn revoke-role!
  "Remove a role from user.
   Queries for the role entity, then retracts it entirely (component cleanup)."
  [conn user-id role]
  (when-let [role-eid (d/q '[:find ?r .
                             :in $ ?uid ?role-name
                             :where [?u :user/id ?uid]
                             [?u :user/roles ?r]
                             [?r :role/name ?role-name]]
                           @conn user-id role)]
    (d/transact conn [[:db/retractEntity role-eid]])
    (mu/log ::role-revoked :user-id user-id :role role)))

;;=============================================================================
;; Collection Constructor
;;=============================================================================

(defrecord UserRolesDataSource [conn user-id]
  coll/DataSource
  (fetch [_ query]
    (let [role-name (:role/name query)
          roles (get-user-roles conn user-id)]
      (when (contains? roles role-name)
        {:role/name role-name})))
  (list-all [_]
    (map (fn [r] {:role/name r}) (get-user-roles conn user-id)))
  (create! [_ data]
    (grant-role! conn user-id (:role/name data))
    {:role/name (:role/name data)})
  (update! [_ _query _data] nil)
  (delete! [_ query]
    (let [role (:role/name query)]
      (if (contains? (get-user-roles conn user-id) role)
        (do (revoke-role! conn user-id role) true)
        false))))

(defn user-roles
  "Create a roles collection for a specific user.
   Supports CRUD: list roles, check role, grant (create), revoke (delete)."
  [conn user-id]
  (coll/collection (->UserRolesDataSource conn user-id)
                   {:id-key :role/name :indexes #{#{:role/name}}}))
