(ns sg.flybot.flybot-site.oauth
  "Google OAuth middleware for blog authentication.

   Endpoints created:
   - GET /oauth2/google - Start OAuth flow (handled by ring-oauth2)
   - GET /oauth2/google/callback - OAuth callback (handled by ring-oauth2)
   - GET /oauth2/google/success - Profile fetch and redirect (our handler)
   - GET /logout - Clear session

   Flow:
   1. User clicks login -> /oauth2/google
   2. ring-oauth2 redirects to Google
   3. Google redirects back to /oauth2/google/callback
   4. ring-oauth2 exchanges code for token, stores in session
   5. ring-oauth2 redirects to /oauth2/google/success (landing-uri)
   6. Our success handler fetches profile, stores user info, redirects to /

   Usage:
   (-> handler
       wrap-logout
       (wrap-oauth-success {:allowed-email-pattern #\".*@mycompany\\\\.com\"})
       (wrap-google-auth {:client-id \"...\"
                          :client-secret \"...\"
                          :redirect-uri \"http://localhost:8080/oauth2/google/callback\"})
       (wrap-session {:store (cookie-store {:key ...})}))

   The :allowed-email-pattern option restricts which Google accounts can log in.
   Users whose email doesn't match will be logged out and shown an error."
  (:require
   [ring.middleware.oauth2 :refer [wrap-oauth2]]
   [clj-http.client :as http]
   [sg.flybot.flybot-site.db :as db]
   [com.brunobonacci.mulog :as mu]))

;;=============================================================================
;; Google Profile Fetching
;;=============================================================================

(defn- fetch-google-profile
  "Fetch user profile from Google userinfo API using access token.
   Returns map with :email, :name, :picture or throws on failure."
  [access-token]
  (mu/log ::profile-fetch-start)
  (let [{:keys [status body]} (try
                                (http/request
                                 {:content-type :json
                                  :accept :json
                                  :url "https://www.googleapis.com/oauth2/v2/userinfo"
                                  :method :get
                                  :oauth-token access-token
                                  :as :json})
                                (catch Exception e
                                  (mu/log ::profile-fetch-error :error (ex-message e))
                                  (throw (ex-info "Could not fetch Google user info"
                                                  {:type :oauth/fetch-profile-failed
                                                   :cause (ex-message e)}))))]
    (if (= status 200)
      (do
        (mu/log ::profile-fetch-success :email (:email body))
        body)
      (do
        (mu/log ::profile-fetch-error :status status)
        (throw (ex-info "Google API returned error"
                        {:type :oauth/google-api-error
                         :status status}))))))

;;=============================================================================
;; Helpers
;;=============================================================================

(defn- redirect-302
  "Create a 302 redirect response."
  [location]
  {:status 302
   :headers {"Location" location}
   :body ""})

(defn- email-allowed?
  "Check if email matches the allowed pattern. Pattern is required."
  [email allowed-pattern]
  (boolean
   (and allowed-pattern
        email
        (re-matches allowed-pattern email))))

;;=============================================================================
;; Middleware
;;=============================================================================

(defn- wrap-oauth-debug
  "Debug wrapper to log OAuth-related requests and responses."
  [handler]
  (fn [request]
    (let [uri (:uri request)
          oauth-path? (and uri (.startsWith uri "/oauth2"))
          debug-path? (= uri "/debug-session")
          api-path? (= uri "/api")]
      (when (or oauth-path? debug-path? api-path?)
        (mu/log ::oauth-request
                :uri uri
                :session-keys (keys (:session request))
                :user-email (get-in request [:session :user-email])
                :has-tokens (some? (get-in request [:session :ring.middleware.oauth2/access-tokens]))
                :cookie (get-in request [:headers "cookie"])))
      (let [response (handler request)]
        (when (or oauth-path? debug-path?)
          (mu/log ::oauth-response
                  :uri uri
                  :status (:status response)
                  :has-session (contains? response :session)
                  :session-keys (keys (:session response))
                  :user-email (get-in response [:session :user-email])))
        response))))

(defn wrap-google-auth
  "Wrap handler with Google OAuth2 authentication.

   Config:
   - :client-id - Google OAuth client ID
   - :client-secret - Google OAuth client secret
   - :base-url - Base URL for constructing redirect-uri (e.g., \"http://localhost:8080\")

   Creates routes:
   - /oauth2/google - Initiates OAuth flow
   - /oauth2/google/callback - Receives Google callback
   - After callback, redirects to /oauth2/google/success (landing-uri)"
  [handler {:keys [client-id client-secret base-url]}]
  (if (and client-id client-secret base-url)
    (let [redirect-uri (str base-url "/oauth2/google/callback")]
      (mu/log ::oauth-configured :redirect-uri redirect-uri)
      (-> handler
          (wrap-oauth2 {:google
                        {:authorize-uri "https://accounts.google.com/o/oauth2/v2/auth"
                         :access-token-uri "https://oauth2.googleapis.com/token"
                         :client-id client-id
                         :client-secret client-secret
                         :scopes ["openid" "email" "profile"]
                         :launch-uri "/oauth2/google"
                         :redirect-uri redirect-uri
                         :landing-uri "/oauth2/google/success"}})
          wrap-oauth-debug))
    (do
      (mu/log ::oauth-disabled :reason "missing credentials")
      handler)))

(defn wrap-oauth-success
  "Handle the /oauth2/google/success route after OAuth callback completes.

   This middleware intercepts requests to /oauth2/google/success, fetches
   the Google profile using the access token from session, and:
   - If email is allowed: creates/updates user in DB, stores user info in session, redirects to /
   - If email not allowed: clears session, redirects with error

   Options:
   - :allowed-email-pattern - Regex pattern for allowed emails.
   - :client-root-path - Where to redirect after success (default: \"/\")
   - :conn - Datahike connection for user persistence"
  ([handler] (wrap-oauth-success handler {}))
  ([handler {:keys [allowed-email-pattern client-root-path conn]
             :or {client-root-path "/"}}]
   (fn [request]
     (if (= "/oauth2/google/success" (:uri request))
       ;; Handle OAuth success - fetch profile and redirect
       (let [session-keys (keys (:session request))
             has-tokens (some? (get-in request [:session :ring.middleware.oauth2/access-tokens]))
             _ (mu/log ::success-handler-start :session-keys session-keys :has-tokens has-tokens)
             access-token (get-in request [:session :ring.middleware.oauth2/access-tokens :google :token])]
         (if access-token
           (try
             (let [profile (fetch-google-profile access-token)
                   email (:email profile)]
               (if (email-allowed? email allowed-email-pattern)
                 ;; Email allowed - upsert user to DB, store in session, redirect
                 (let [user-id (:id profile)
                       _ (when conn
                           (db/upsert-user! conn #:user{:id user-id
                                                        :email email
                                                        :name (:name profile)
                                                        :picture (or (:picture profile) "")}))
                       session (-> (:session request)
                                   (assoc :user-id user-id
                                          :user-email email
                                          :user-name (:name profile)
                                          :user-picture (:picture profile)))]
                   (mu/log ::user-logged-in :name (:name profile))
                   (-> (redirect-302 client-root-path)
                       (assoc :session session)))
                 ;; Email not allowed - clear session and show error
                 (do
                   (mu/log ::login-denied :email email :reason "email not allowed")
                   (-> (redirect-302 (str client-root-path "?error=unauthorized"))
                       (assoc :session nil)))))
             (catch Exception e
               (mu/log ::success-handler-error :error (ex-message e) :type (type e))
               (-> (redirect-302 (str client-root-path "?error=oauth-failed"))
                   (assoc :session nil))))
           ;; No token in session - something went wrong
           (do
             (mu/log ::no-token-in-session :session-keys session-keys)
             (-> (redirect-302 (str client-root-path "?error=no-token"))
                 (assoc :session nil)))))
       ;; Not the success route - pass through
       (handler request)))))

(defn wrap-logout
  "Handle /logout by clearing session and redirecting to home."
  [handler]
  (fn [request]
    (if (= "/logout" (:uri request))
      (do
        (mu/log ::user-logout :name (get-in request [:session :user-name]))
        {:status 302
         :headers {"Location" "/"}
         :session nil})
      (handler request))))

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

  ;; wrap-logout redirects on /logout
  (let [handler (wrap-logout (fn [_] {:status 200}))
        response (handler {:uri "/logout" :session {:user-email "test@example.com"}})]
    [(:status response) (get-in response [:headers "Location"]) (:session response)])
  ;=> [302 "/" nil]

  ;; wrap-logout passes through other requests
  (let [handler (wrap-logout (fn [_] {:status 200 :body "ok"}))
        response (handler {:uri "/api" :session {:user-email "test@example.com"}})]
    (:status response))
  ;=> 200

  ;; wrap-google-auth passes through when not configured
  (let [handler (wrap-google-auth identity {})
        result (handler {:uri "/test"})]
    (:uri result))
  ;=> "/test"

  ;; wrap-oauth-success passes through non-success routes
  (let [handler (wrap-oauth-success (fn [_] {:status 200 :body "ok"}))
        response (handler {:uri "/api" :session {}})]
    (:status response))
  ;=> 200

  ;; wrap-oauth-success redirects with error when no token
  (let [handler (wrap-oauth-success (fn [_] {:status 200}))
        response (handler {:uri "/oauth2/google/success" :session {}})]
    [(:status response) (get-in response [:headers "Location"])])
  ;=> [302 "/?error=no-token"]
  )
