(ns sg.flybot.flybot-site.oauth
  "Google OAuth middleware for blog authentication.

   Endpoints created:
   - GET /oauth2/google - Start OAuth flow
   - GET /oauth2/google/callback - OAuth callback
   - GET /logout - Clear session

   Usage:
   (-> handler
       wrap-logout
       (wrap-fetch-profile {:allowed-email-pattern #\".*@mycompany\\\\.com\"})
       (wrap-google-auth {:client-id \"...\"
                          :client-secret \"...\"
                          :base-url \"http://localhost:8080\"})
       (wrap-session {:store (memory-store)}))

   The :allowed-email-pattern option restricts which Google accounts can log in.
   Users whose email doesn't match will be redirected with an error."
  (:require
   [ring.middleware.oauth2 :refer [wrap-oauth2]]
   [clj-http.client :as http]
   [cheshire.core :as json]
   [sg.flybot.flybot-site.log :as log]))

;;=============================================================================
;; Google Profile Fetching
;;=============================================================================

(defn- fetch-google-profile
  "Fetch user profile from Google People API using access token."
  [access-token]
  (try
    (let [response (http/get "https://www.googleapis.com/oauth2/v2/userinfo"
                             {:headers {"Authorization" (str "Bearer " access-token)}
                              :as :json})]
      (log/debug "Google profile response:" (:body response))
      (:body response))
    (catch Exception e
      (log/error "Failed to fetch Google profile:" (ex-message e))
      nil)))

;;=============================================================================
;; Middleware
;;=============================================================================

(defn wrap-google-auth
  "Wrap handler with Google OAuth2 authentication.

   Config:
   - :client-id - Google OAuth client ID
   - :client-secret - Google OAuth client secret
   - :base-url - Base URL of the application (e.g., \"http://localhost:8080\")"
  [handler {:keys [client-id client-secret base-url]}]
  (if (and client-id client-secret)
    (wrap-oauth2 handler
                 {:google
                  {:authorize-uri "https://accounts.google.com/o/oauth2/v2/auth"
                   :access-token-uri "https://oauth2.googleapis.com/token"
                   :client-id client-id
                   :client-secret client-secret
                   :scopes ["openid" "email" "profile"]
                   :launch-uri "/oauth2/google"
                   :redirect-uri (str base-url "/oauth2/google/callback")
                   :landing-uri "/"}})
    (do
      (log/warn "Google OAuth not configured - authentication disabled")
      handler)))

(defn- email-allowed?
  "Check if email matches the allowed pattern."
  [email allowed-pattern]
  (boolean
   (or (nil? allowed-pattern)
       (and email (re-matches allowed-pattern email)))))

(defn wrap-fetch-profile
  "After OAuth callback, fetch Google profile and store in session.

   Reads OAuth2 token from session, fetches profile, and stores:
   - :user-email
   - :user-name
   - :user-picture

   Options:
   - :allowed-email-pattern - Regex pattern for allowed emails.
     Users whose email doesn't match will be logged out and shown an error."
  ([handler] (wrap-fetch-profile handler {}))
  ([handler {:keys [allowed-email-pattern]}]
   (fn [request]
     (let [access-token (get-in request [:session :ring.middleware.oauth2/access-tokens :google :token])]
       (if (and access-token
                (not (get-in request [:session :user-email])))
         ;; Have OAuth token but haven't fetched profile yet
         (let [profile (fetch-google-profile access-token)
               email (:email profile)]
           (if (email-allowed? email allowed-email-pattern)
             ;; Email allowed - store in session
             (let [session (-> (:session request)
                               (assoc :user-email email
                                      :user-name (:name profile)
                                      :user-picture (:picture profile)))]
               (log/info "User logged in:" email)
               (-> (handler (assoc request :session session))
                   (assoc :session session)))
             ;; Email not allowed - clear session and redirect with error
             (do
               (log/warn "Login denied - email not allowed:" email)
               {:status 302
                :headers {"Location" "/?error=unauthorized"}
                :session nil})))
         (handler request))))))

(defn wrap-logout
  "Handle /logout by clearing session and redirecting to home."
  [handler]
  (fn [request]
    (if (= "/logout" (:uri request))
      (do
        (log/info "User logged out:" (get-in request [:session :user-email]))
        {:status 302
         :headers {"Location" "/"}
         :session nil})
      (handler request))))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; email-allowed? returns true when no pattern
  (email-allowed? "anyone@example.com" nil)
  ;=> true

  ;; email-allowed? returns true when email matches pattern
  (email-allowed? "user@mycompany.com" #".*@mycompany\.com")
  ;=> true

  ;; email-allowed? returns false when email doesn't match pattern
  (email-allowed? "user@other.com" #".*@mycompany\.com")
  ;=> false

  ;; email-allowed? returns false for nil email with pattern
  (email-allowed? nil #".*@mycompany\.com")
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
    (:uri result)))
  ;=> "/test")
