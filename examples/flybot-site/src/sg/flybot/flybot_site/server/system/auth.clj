(ns sg.flybot.flybot-site.server.system.auth
  "Authentication using oie: Google OAuth2, role initialization, session identity.

   Endpoints created by oie's wrap-oauth2:
   - GET /oauth2/google - Start OAuth flow (ring-oauth2)
   - GET /oauth2/google/callback - OAuth callback (ring-oauth2)
   - GET /oauth2/google/success - oie intercepts, calls fetch-profile-fn + login-fn

   Flow:
   1. User clicks login -> /oauth2/google
   2. ring-oauth2 redirects to Google
   3. Google redirects back to /oauth2/google/callback
   4. ring-oauth2 exchanges code for tokens (including id_token), stores in session
   5. ring-oauth2 redirects to /oauth2/google/success (landing-uri)
   6. oie intercepts: decode id_token via fetch-profile-fn, validate via login-fn
   7. login-fn checks email, upserts user, initializes roles, returns identity
   8. oie stores identity in session under ::oie-session/user, redirects to /

   ## Authentication

   Uses oie/wrap-authenticate with two strategies:
   - Session strategy: reads identity from session cookie (logged-in users)
   - Anonymous strategy: fallback that returns {} (guests)

   This means oie/get-identity always returns a value — real identity or {}.
   with-role naturally rejects guests (no :roles key → has-role? returns false).

   ## Role Initialization

   On first login, users are granted initial roles based on owner-emails config:
   - If email in owner-emails: granted #{:member :admin :owner}
   - Otherwise: granted #{:member}

   Returning users get their existing roles from the database.
   Roles are stored in the session cookie (no DB lookup on each request)."
  (:require
   [flybot.oie.core :as oie]
   [flybot.oie.oauth2 :as oie-oauth2]
   [flybot.oie.session :as oie-session]
   [flybot.oie.strategy.session :as oie-session-strategy]
   [sg.flybot.flybot-site.server.system.db :as db]
   [com.brunobonacci.mulog :as mu]))

;;=============================================================================
;; Strategies
;;=============================================================================

(defn- anonymous-strategy
  "Fallback strategy that always authenticates as anonymous (empty identity).
   Placed last in the strategy chain so guests pass through wrap-authenticate
   instead of getting 401. with-role rejects guests naturally (no :roles key)."
  []
  {:authenticate (fn [_] {:authenticated {}})})

;;=============================================================================
;; Helpers
;;=============================================================================

(defn- email-allowed?
  "Check if email matches the allowed pattern. Pattern is required."
  [email allowed-pattern]
  (boolean
   (and allowed-pattern
        email
        (re-matches allowed-pattern email))))

;;=============================================================================
;; Role Initialization
;;=============================================================================

(defn- initialize-roles!
  "Initialize roles for a user on login.

   On first login, grants initial roles based on owner-emails config:
   - If email in owner-emails: grants #{:member :admin :owner}
   - Otherwise: grants #{:member}

   Returning users get their existing roles from the database.
   Returns the set of role keywords to store in session."
  [conn user-id email owner-emails]
  (let [existing-roles (db/get-user-roles conn user-id)]
    (if (empty? existing-roles)
      ;; First login - grant initial roles
      (let [is-owner? (contains? (or owner-emails #{}) email)]
        (if is-owner?
          ;; Owner gets all 3 roles
          (do (db/grant-role! conn user-id :member)
              (db/grant-role! conn user-id :admin)
              (db/grant-role! conn user-id :owner)
              (mu/log ::initial-roles-granted :user-id user-id :roles #{:member :admin :owner} :reason :owner-email)
              #{:member :admin :owner})
          ;; Regular user gets member only
          (do (db/grant-role! conn user-id :member)
              (mu/log ::initial-roles-granted :user-id user-id :roles #{:member} :reason :first-login)
              #{:member})))
      ;; Returning user - use existing roles
      (do (mu/log ::existing-roles-loaded :user-id user-id :roles existing-roles)
          existing-roles))))

;;=============================================================================
;; OAuth2 Integration
;;=============================================================================

(defn make-login-fn
  "Create the login function for oie's OAuth2 flow.

   Receives decoded JWT profile from Google's id_token (via fetch-profile-fn).
   Validates email against allowed pattern, upserts user to DB, initializes roles.
   Returns identity map on success, nil on rejection (oie returns 403).

   The identity map shape:
   {:user-id     \"google-sub-id\"
    :user-email  \"user@example.com\"
    :user-name   \"Display Name\"
    :user-picture \"https://...\"
    :roles       #{:member}}"
  [{:keys [allowed-email-pattern owner-emails conn]}]
  (fn [{:keys [sub email name picture] :as _jwt-claims}]
    (if-not (email-allowed? email allowed-email-pattern)
      (do (mu/log ::login-denied :email email :reason "email not allowed")
          nil)
      (do
        (when conn
          (db/upsert-user! conn #:user{:id sub
                                       :email email
                                       :name name
                                       :picture (or picture "")}))
        (let [roles (if conn
                      (initialize-roles! conn sub email owner-emails)
                      #{:member})]
          (mu/log ::user-logged-in :name name :roles roles)
          {:user-id sub
           :user-email email
           :user-name name
           :user-picture picture
           :roles roles})))))

(defn make-oauth2-profiles
  "Build oie-compatible OAuth2 profiles for Google authentication.

   Returns a profiles map for oie-oauth2/wrap-oauth2, or nil if credentials
   are missing (disables OAuth in dev mode without credentials).

   The profile uses decode-id-token to extract user info from Google's JWT
   instead of making an HTTP call to the userinfo API. Google's id_token
   :sub claim is the same value as the userinfo :id field."
  [{:keys [client-id client-secret base-url client-root-path]
    :or {client-root-path "/"}}
   login-fn]
  (when (and client-id client-secret base-url)
    (let [redirect-uri (str base-url "/oauth2/google/callback")]
      (mu/log ::oauth-configured :redirect-uri redirect-uri)
      {:google
       {:authorize-uri       "https://accounts.google.com/o/oauth2/v2/auth"
        :access-token-uri    "https://oauth2.googleapis.com/token"
        :client-id           client-id
        :client-secret       client-secret
        :scopes              ["openid" "email" "profile"]
        :launch-uri          "/oauth2/google"
        :redirect-uri        redirect-uri
        :landing-uri         "/oauth2/google/success"
        :fetch-profile-fn    (fn [tokens]
                               (oie-oauth2/decode-id-token (:id-token tokens)))
        :login-fn            login-fn
        :success-redirect-uri client-root-path}})))

;;=============================================================================
;; Middleware
;;=============================================================================

(defn wrap-authenticate
  "Wrap handler with oie authentication (session + anonymous fallback).
   Always succeeds — guests get {} identity, logged-in users get real identity."
  [handler]
  (oie/wrap-authenticate handler
                         [(oie-session-strategy/session-strategy)
                          (anonymous-strategy)]))

(defn wrap-oauth2
  "Wrap handler with oie's OAuth2 middleware.
   No-op if profiles is nil (OAuth disabled)."
  [handler profiles]
  (if profiles
    (oie-oauth2/wrap-oauth2 handler profiles)
    (do
      (mu/log ::oauth-disabled :reason "missing credentials")
      handler)))

(defn wrap-logout-route
  "Route /logout to oie's POST-only logout handler.
   Returns 405 for non-POST requests (CSRF protection)."
  [handler {:keys [redirect-uri] :or {redirect-uri "/"}}]
  (let [logout-fn (oie-session/logout-handler {:redirect-uri redirect-uri})]
    (fn [request]
      (if (= "/logout" (:uri request))
        (do (when (= :post (:request-method request))
              (mu/log ::user-logout
                      :name (:user-name (get-in request [:session oie-session/session-key]))))
            (logout-fn request))
        (handler request)))))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; email-allowed? returns false when no pattern (secure default)
  (email-allowed? "anyone@example.com" nil)
  ;=> false

  ;; email-allowed? returns true when email matches pattern
  (email-allowed? "user@mycompany.com" #".*@mycompany\.com")
  ;=> true

  ;; email-allowed? returns false when email doesn't match pattern
  (email-allowed? "user@other.com" #".*@mycompany\.com")
  ;=> false

  ;; email-allowed? returns false for nil email with pattern
  (email-allowed? nil #".*@mycompany\.com")
  ;=> false

  ;; email-allowed? returns false for nil email and nil pattern
  (email-allowed? nil nil)
  ;=> false

  ;; wrap-authenticate always succeeds (anonymous fallback)
  (let [handler (wrap-authenticate (fn [req] {:identity (oie/get-identity req)}))
        resp (handler {:session {}})]
    (:identity resp))
  ;=> {}

  ;; wrap-authenticate returns real identity for logged-in user
  (let [handler (wrap-authenticate (fn [req] {:identity (oie/get-identity req)}))
        ident {:user-id "u1" :roles #{:member}}
        resp (handler {:session {::oie-session/user ident}})]
    (:identity resp))
  ;=> {:user-id "u1" :roles #{:member}}

  ;; wrap-logout-route returns 405 for GET (CSRF protection)
  (let [handler (wrap-logout-route (fn [_] {:status 200}) {})
        response (handler {:uri "/logout" :request-method :get})]
    [(:status response) (get-in response [:headers "Allow"])])
  ;=> [405 "POST"]

  ;; wrap-logout-route clears session on POST
  (let [handler (wrap-logout-route (fn [_] {:status 200}) {})
        response (handler {:uri "/logout" :request-method :post
                           :session {::oie-session/user {:user-name "Test"}}})]
    [(:status response) (get-in response [:headers "Location"]) (:session response)])
  ;=> [302 "/" nil]

  ;; wrap-logout-route with custom redirect-uri
  (let [handler (wrap-logout-route (fn [_] {:status 200}) {:redirect-uri "/home"})
        response (handler {:uri "/logout" :request-method :post})]
    (get-in response [:headers "Location"]))
  ;=> "/home"

  ;; wrap-logout-route passes through other requests
  (let [handler (wrap-logout-route (fn [_] {:status 200 :body "ok"}) {})
        response (handler {:uri "/api" :request-method :get})]
    (:status response))
  ;=> 200

  ;; wrap-oauth2 passes through when profiles is nil
  (let [handler (wrap-oauth2 identity nil)
        result (handler {:uri "/test"})]
    (:uri result))
  ;=> "/test"

  ;; make-oauth2-profiles returns nil without credentials
  (make-oauth2-profiles {} (fn [_] nil))
  ;=> nil

  ;; make-oauth2-profiles builds profile with all required keys
  (let [profiles (make-oauth2-profiles {:client-id "id"
                                        :client-secret "secret"
                                        :base-url "http://localhost:8080"}
                                       (fn [_] {:user-id "1"}))]
    [(some? (:google profiles))
     (:launch-uri (:google profiles))
     (:redirect-uri (:google profiles))
     (:landing-uri (:google profiles))
     (:success-redirect-uri (:google profiles))
     (fn? (:fetch-profile-fn (:google profiles)))
     (fn? (:login-fn (:google profiles)))])
  ;=> [true "/oauth2/google" "http://localhost:8080/oauth2/google/callback" "/oauth2/google/success" "/" true true]
  )

^:rct/test
(comment
  ;; === Role initialization tests ===
  (def conn (db/create-conn!))

  ;; Create a regular user (not in owner-emails)
  (db/create-user! conn #:user{:id "user-1" :email "user@example.com" :name "User" :picture ""})

  ;; First login grants :member role
  (#'initialize-roles! conn "user-1" "user@example.com" #{})
  ;=> #{:member}

  (db/get-user-roles conn "user-1") ;=> #{:member}

  ;; Subsequent logins return existing roles
  (#'initialize-roles! conn "user-1" "user@example.com" #{})
  ;=> #{:member}

  ;; Create an owner user
  (db/create-user! conn #:user{:id "owner-1" :email "owner@example.com" :name "Owner" :picture ""})

  ;; First login as owner grants all roles
  (#'initialize-roles! conn "owner-1" "owner@example.com" #{"owner@example.com"})
  ;=> #{:member :admin :owner}

  (db/get-user-roles conn "owner-1") ;=> #{:member :admin :owner}

  ;; Subsequent logins return existing roles
  (#'initialize-roles! conn "owner-1" "owner@example.com" #{"owner@example.com"})
  ;=> #{:member :admin :owner}

  (db/release-conn! conn)

  ;; === make-login-fn tests ===
  (def conn2 (db/create-conn!))

  ;; login-fn rejects disallowed email
  (let [login (make-login-fn {:allowed-email-pattern #".*@allowed\.com"
                              :owner-emails #{}
                              :conn conn2})]
    (login {:sub "u1" :email "user@rejected.com" :name "User" :picture ""}))
  ;=> nil

  ;; login-fn accepts allowed email and returns identity
  (let [login (make-login-fn {:allowed-email-pattern #".*@allowed\.com"
                              :owner-emails #{}
                              :conn conn2})
        ident (login {:sub "u2" :email "user@allowed.com" :name "User" :picture "/pic.png"})]
    [(:user-id ident) (:user-email ident) (:roles ident)])
  ;=> ["u2" "user@allowed.com" #{:member}]

  ;; login-fn grants owner roles
  (let [login (make-login-fn {:allowed-email-pattern #".*@allowed\.com"
                              :owner-emails #{"boss@allowed.com"}
                              :conn conn2})
        ident (login {:sub "u3" :email "boss@allowed.com" :name "Boss" :picture ""})]
    (:roles ident))
  ;=> #{:member :admin :owner}

  ;; login-fn works without conn (no DB)
  (let [login (make-login-fn {:allowed-email-pattern #".*"
                              :owner-emails #{}
                              :conn nil})
        ident (login {:sub "u4" :email "any@any.com" :name "Any" :picture ""})]
    [(:user-id ident) (:roles ident)])
  ;=> ["u4" #{:member}]

  (db/release-conn! conn2))
