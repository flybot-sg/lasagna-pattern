(ns sg.flybot.flybot-site.server.system.cfg
  "Configuration schema and validation using Malli.

   Modes:
   - :prod (default) - requires OAuth + session secret
   - :dev-with-oauth2 - requires OAuth, tests OAuth locally
   - :dev - auto-login fallback, no OAuth needed

   Config structure:
   ```clojure
   {:mode :prod  ; or :dev, :dev-with-oauth2
    :server {:port 8080 :base-url \"http://localhost:8080\"}
    :db {:backend :mem :id \"blog\"}
    :auth {:google-client-id \"...\" :google-client-secret \"...\"}
    :session {:secret \"32-hex-chars\" :timeout 43200}}
   ```"
  (:require
   [malli.core :as m]
   [malli.util :as mu]
   #?(:clj [clojure.string :as str])))

;;=============================================================================
;; Schema Registries
;;=============================================================================

(def server-registry
  {::server
   [:map
    [:port {:optional true} :int]
    [:base-url {:optional true} :string]]})

(def db-registry
  {::db
   [:multi {:dispatch :backend :default :mem}
    [:mem [:map
           [:backend [:= :mem]]
           [:id {:optional true} :string]]]
    [:file [:map
            [:backend [:= :file]]
            [:path :string]]]
    [:s3 [:map
          [:backend [:= :s3]]
          [:bucket :string]
          [:region :string]]]]})

(def auth-registry
  {::auth
   [:map
    [:google-client-id {:optional true} [:maybe :string]]
    [:google-client-secret {:optional true} [:maybe :string]]
    [:owner-emails {:optional true} [:maybe [:or :string [:set :string]]]]
    [:allowed-email-pattern {:optional true} [:maybe :string]]]})

(def session-registry
  {::session
   [:map
    [:secret {:optional true} [:maybe :string]]
    [:timeout {:optional true} [:maybe :int]]]})

(def mode-registry
  {::mode [:enum :prod :dev :dev-with-oauth2]})

(def init-registry
  {::init
   [:map
    [:seed? {:optional true} [:maybe :boolean]]]})

(def log-registry
  {::log
   [:map
    [:publishers {:optional true}
     [:vector [:map
               [:type :keyword]
               [:filename {:optional true} :string]]]]
    [:context {:optional true} :map]]})

(def uploads-registry
  {::uploads
   [:multi {:dispatch :type :default :local}
    [:local [:map
             [:type [:= :local]]
             [:dir {:optional true} :string]]]
    [:s3 [:map
          [:type [:= :s3]]
          [:bucket :string]
          [:region :string]]]]})

;; Mode-specific config validation using multi-dispatch
;; :prod requires OAuth + session secret
;; :dev-with-oauth2 requires OAuth only
;; :dev requires nothing (auto-login fallback)
(def config-registry
  {::config
   [:multi {:dispatch :mode :default :prod}
    [:prod
     [:map
      [:mode [:= :prod]]
      [:server {:optional true} ::server]
      [:db {:optional true} ::db]
      [:auth [:map
              [:google-client-id :string]
              [:google-client-secret :string]
              [:owner-emails {:optional true} [:maybe [:or :string [:set :string]]]]
              [:allowed-email-pattern {:optional true} [:maybe :string]]]]
      [:session [:map
                 [:secret :string]
                 [:timeout {:optional true} [:maybe :int]]]]
      [:init {:optional true} ::init]
      [:log {:optional true} ::log]
      [:uploads {:optional true} ::uploads]]]

    [:dev-with-oauth2
     [:map
      [:mode [:= :dev-with-oauth2]]
      [:server {:optional true} ::server]
      [:db {:optional true} ::db]
      [:auth [:map
              [:google-client-id :string]
              [:google-client-secret :string]
              [:owner-emails {:optional true} [:maybe [:or :string [:set :string]]]]
              [:allowed-email-pattern {:optional true} [:maybe :string]]]]
      [:session {:optional true} ::session]
      [:init {:optional true} ::init]
      [:log {:optional true} ::log]
      [:uploads {:optional true} ::uploads]]]

    [:dev
     [:map
      [:mode [:= :dev]]
      [:server {:optional true} ::server]
      [:db {:optional true} ::db]
      [:auth {:optional true} ::auth]
      [:session {:optional true} ::session]
      [:init {:optional true} ::init]
      [:log {:optional true} ::log]
      [:uploads {:optional true} ::uploads]]]]})

;;=============================================================================
;; Combined Registry
;;=============================================================================

(def registry
  (merge
   (m/default-schemas)
   (mu/schemas)
   server-registry
   db-registry
   auth-registry
   session-registry
   mode-registry
   init-registry
   log-registry
   uploads-registry
   config-registry))

;;=============================================================================
;; Validation
;;=============================================================================

(defn schema-of [elem]
  (m/schema elem {:registry registry}))

(def validator
  (comp m/validator schema-of))

(def explainer
  (comp m/explainer schema-of))

(defn validate-cfg
  "Validates the given config.
   If validation passes, returns the config.
   Else, throws an exception with error details."
  [cfg]
  (if ((validator ::config) cfg)
    cfg
    (throw
     (ex-info "Invalid config provided"
              {:errors (m/explain (schema-of ::config) cfg)}))))

;;=============================================================================
;; Config Parsing Helpers
;;=============================================================================

#?(:clj
   (defn parse-owner-emails
     "Parse comma-separated emails to set."
     [s]
     (cond
       (set? s) s
       (or (nil? s) (str/blank? s)) #{}
       :else (set (map str/trim (str/split s #","))))))

#?(:clj
   (defn parse-email-pattern
     "Parse regex pattern string."
     [s]
     (when (and s (not (str/blank? s)))
       (re-pattern s))))

#?(:clj
   (defn parse-session-secret
     "Parse 32 hex chars to 16-byte array."
     [s]
     (when (and s (not (str/blank? s)))
       (let [hex-chars (str/replace s #"[^0-9a-fA-F]" "")]
         (when (not= 32 (count hex-chars))
           (throw (ex-info "Session secret must be 32 hex characters (16 bytes)"
                           {:provided-length (count hex-chars)})))
         (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 16))
                          (partition 2 hex-chars)))))))

;;=============================================================================
;; Config Defaults
;;=============================================================================

(def defaults
  "Default configuration values."
  {:mode :prod
   :server {:port 8080
            :base-url "http://localhost:8080"}
   :db {:backend :mem
        :id "blog"}
   :auth {:owner-emails #{}}
   :session {:timeout 43200}
   :init {:seed? false}
   :log {:publishers [{:type :simple-file :filename "logs/flybot.log"}]
         :context {:app "flybot-site"}}
   :uploads {:type :local :dir "resources/public/uploads"}})

(defn apply-defaults
  "Deep merge config with defaults."
  [cfg]
  (letfn [(deep-merge [a b]
            (if (and (map? a) (map? b))
              (merge-with deep-merge a b)
              (if (some? b) b a)))]
    (deep-merge defaults cfg)))

;;=============================================================================
;; Environment Config (CLJ only)
;;=============================================================================

#?(:clj
   (defn- get-env
     ([k] (System/getenv k))
     ([k default] (or (System/getenv k) default))))

#?(:clj
   (defn- parse-mode
     "Parse BLOG_MODE env var to keyword. Returns nil if invalid."
     [s]
     (#{:dev :dev-with-oauth2 :prod} (keyword s))))

#?(:clj
   (defn config-from-env
     "Build config map from environment variables.

      Environment variables:
      - BLOG_MODE (:prod, :dev, :dev-with-oauth2) - defaults to :prod
      - BLOG_PORT, BLOG_BASE_URL
      - DATAHIKE_BACKEND (:mem, :file, :s3), DATAHIKE_PATH, DATAHIKE_BUCKET, DATAHIKE_REGION, DATAHIKE_ID
      - GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET
      - BLOG_OWNER_EMAILS, BLOG_ALLOWED_EMAILS
      - SESSION_SECRET, SESSION_TIMEOUT
      - BLOG_SEED"
     []
     (cond->
      {:server {:port (some-> (get-env "BLOG_PORT") parse-long)
                :base-url (get-env "BLOG_BASE_URL")}
       :db {:backend (some-> (get-env "DATAHIKE_BACKEND") keyword)
            :path (get-env "DATAHIKE_PATH")
            :bucket (get-env "DATAHIKE_BUCKET")
            :region (get-env "DATAHIKE_REGION")
            :id (get-env "DATAHIKE_ID")}
       :auth {:google-client-id (get-env "GOOGLE_CLIENT_ID")
              :google-client-secret (get-env "GOOGLE_CLIENT_SECRET")
              :owner-emails (get-env "BLOG_OWNER_EMAILS")
              :allowed-email-pattern (get-env "BLOG_ALLOWED_EMAILS")}
       :session {:secret (get-env "SESSION_SECRET")
                 :timeout (some-> (get-env "SESSION_TIMEOUT") parse-long)}
       :init {:seed? (= "true" (get-env "BLOG_SEED"))}
       :uploads (if-let [bucket (get-env "S3_UPLOADS_BUCKET")]
                  {:type :s3
                   :bucket bucket
                   :region (or (get-env "S3_UPLOADS_REGION") "ap-southeast-1")}
                  {:type :local
                   :dir (or (get-env "UPLOADS_DIR") "resources/public/uploads")})}
       (parse-mode (get-env "BLOG_MODE")) (assoc :mode (parse-mode (get-env "BLOG_MODE"))))))

#?(:clj
   (defn prepare-cfg
     "Prepare config: apply defaults, parse special fields, validate.
      Validation is mode-aware (Malli schema enforces requirements per mode).
      Returns config ready for make-system."
     [cfg]
     (let [merged (apply-defaults cfg)
           with-parsed-emails (update-in merged [:auth :owner-emails] parse-owner-emails)]
       (validate-cfg with-parsed-emails)
       with-parsed-emails)))

^:rct/test
(comment
  ;; Defaults
  (:mode defaults) ;=> :prod
  (:server defaults) ;=> {:port 8080 :base-url "http://localhost:8080"}

  ;; Mode validation - :dev allows everything optional
  ((validator ::config) {:mode :dev}) ;=> true
  ((validator ::config) {:mode :dev :server {:port 8080}}) ;=> true

  ;; Mode validation - :dev-with-oauth2 requires OAuth
  ((validator ::config) {:mode :dev-with-oauth2}) ;=> false
  ((validator ::config) {:mode :dev-with-oauth2
                         :auth {:google-client-id "id"
                                :google-client-secret "secret"}}) ;=> true

  ;; Mode validation - :prod requires OAuth + session secret
  ((validator ::config) {:mode :prod}) ;=> false
  ((validator ::config) {:mode :prod
                         :auth {:google-client-id "id"
                                :google-client-secret "secret"}}) ;=> false
  ((validator ::config) {:mode :prod
                         :auth {:google-client-id "id"
                                :google-client-secret "secret"}
                         :session {:secret "abc123"}}) ;=> true

  ;; DB multi-spec: s3 requires bucket+region, file requires path
  ((validator ::config) {:mode :dev :db {:backend :s3}}) ;=> false
  ((validator ::config) {:mode :dev :db {:backend :s3 :bucket "b"}}) ;=> false
  ((validator ::config) {:mode :dev :db {:backend :file}}) ;=> false
  ((validator ::config) {:mode :dev :db {:backend :file :path "/tmp/db"}}) ;=> true
  ((validator ::config) {:mode :dev :db {:backend :s3 :bucket "b" :region "us-east-1"}}) ;=> true

  ;; Apply defaults
  (:mode (apply-defaults {})) ;=> :prod
  (:server (apply-defaults {})) ;=> {:port 8080 :base-url "http://localhost:8080"}
  (:server (apply-defaults {:server {:port 3000}}))
  ;=> {:port 3000 :base-url "http://localhost:8080"}

  ;; Parse owner emails
  (parse-owner-emails nil) ;=> #{}
  (parse-owner-emails "") ;=> #{}
  (parse-owner-emails "a@b.com") ;=> #{"a@b.com"}
  (parse-owner-emails "a@b.com, c@d.com") ;=> #{"a@b.com" "c@d.com"}
  (parse-owner-emails #{"already" "a-set"}) ;=> #{"already" "a-set"}
  )
