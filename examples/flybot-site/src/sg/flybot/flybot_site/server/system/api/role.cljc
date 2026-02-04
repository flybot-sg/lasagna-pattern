(ns sg.flybot.flybot-site.server.system.api.role
  "Role checking helper for the API.

   Roles are stored as a set in the session (e.g., `#{:member :admin}`).

   ## Usage in API data maps

   Use `with-role` to gate access to role-specific data:

   ```clojure
   {:data
    {:posts (read-only posts)  ; public
     :role/member (with-role session :member
                    {:posts member-posts
                     :me user-info})
     :role/admin (with-role session :admin
                   {:posts posts})}}
   ```

   Patterns declare which role they're acting as:
   ```clojure
   '{:posts ?all}                              ; guest read
   '{:role/member {:posts {nil {:title ...}}}} ; member create
   '{:role/admin {:posts {{:id 1} nil}}}       ; admin delete
   ```")

(defn has-role?
  "Check if session has required role."
  [session role]
  (contains? (:roles session) role))

(defn with-role
  "Return data if session has role, nil otherwise.

   For use in pullable data maps where nil means 'not available'.
   Pattern matching against a nil-valued key will fail naturally."
  [session role data]
  (when (has-role? session role)
    data))

^:rct/test
(comment
  (has-role? {:roles #{:member :admin}} :member) ;=> true
  (has-role? {:roles #{:member}} :admin) ;=> false
  (has-role? {:roles nil} :member) ;=> false
  (has-role? {} :member) ;=> false

  ;; with-role returns data when role present
  (with-role {:roles #{:member}} :member {:posts []}) ;=> {:posts []}

  ;; with-role returns nil when role missing
  (with-role {:roles #{:member}} :admin {:posts []}) ;=> nil
  (with-role {} :member {:posts []})) ;=> nil)
