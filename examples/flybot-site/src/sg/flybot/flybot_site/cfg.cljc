(ns sg.flybot.flybot-site.cfg
  "Configuration schema and validation using Malli.

   Config structure:
   ```clojure
   {:server {:port 8080 :base-url \"http://localhost:8080\"}
    :db {:backend :mem :id \"blog\"}
    :auth {:owner-emails #{\"admin@example.com\"}}
    :session {:timeout 43200}
    :dev {:mode? false :seed? true}}
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

(def dev-registry
  {::dev
   [:map
    [:mode? {:optional true} [:maybe :boolean]]
    [:seed? {:optional true} [:maybe :boolean]]
    [:backup-dir {:optional true} [:maybe :string]]]})

(def config-registry
  {::config
   [:map
    [:server {:optional true} ::server]
    [:db {:optional true} ::db]
    [:auth {:optional true} ::auth]
    [:session {:optional true} ::session]
    [:dev {:optional true} ::dev]]})

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
   dev-registry
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
  {:server {:port 8080
            :base-url "http://localhost:8080"}
   :db {:backend :mem
        :id "blog"}
   :auth {:owner-emails #{}}
   :session {:timeout 43200}
   :dev {:mode? false
         :seed? true}})

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
   (defn config-from-env
     "Build config map from environment variables.

      Environment variables:
      - BLOG_PORT, BLOG_BASE_URL
      - DATAHIKE_BACKEND (:mem, :file, :s3), DATAHIKE_PATH, DATAHIKE_BUCKET, DATAHIKE_REGION, DATAHIKE_ID
      - GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET
      - BLOG_OWNER_EMAILS, BLOG_ALLOWED_EMAILS
      - SESSION_SECRET, SESSION_TIMEOUT
      - BLOG_DEV_MODE, BLOG_SEED, BLOG_BACKUP_DIR"
     []
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
      :dev {:mode? (= "true" (get-env "BLOG_DEV_MODE"))
            :seed? (not= "false" (get-env "BLOG_SEED"))
            :backup-dir (get-env "BLOG_BACKUP_DIR")}}))

#?(:clj
   (defn prepare-cfg
     "Prepare config: apply defaults, parse special fields, validate.
      Returns config ready for make-system."
     [cfg]
     (let [merged (apply-defaults cfg)
           ;; Parse owner-emails to set
           with-parsed-emails (update-in merged [:auth :owner-emails] parse-owner-emails)]
       (validate-cfg with-parsed-emails)
       with-parsed-emails)))

^:rct/test
(comment
  ;; Defaults
  (:server defaults) ;=> {:port 8080 :base-url "http://localhost:8080"}

  ;; Validation passes
  ((validator ::config) {}) ;=> true
  ((validator ::config) {:server {:port 8080}}) ;=> true
  ((validator ::config) {:db {:backend :file :path "/tmp/db"}}) ;=> true
  ((validator ::config) {:db {:backend :s3 :bucket "my-bucket" :region "us-east-1"}}) ;=> true

  ;; Validation fails
  ((validator ::config) {:db {:backend :invalid}}) ;=> false
  ((validator ::config) {:server {:port "not-an-int"}}) ;=> false
  ;; Multi-spec: s3 requires bucket+region, file requires path
  ((validator ::config) {:db {:backend :s3}}) ;=> false
  ((validator ::config) {:db {:backend :s3 :bucket "b"}}) ;=> false
  ((validator ::config) {:db {:backend :file}}) ;=> false

  ;; Apply defaults
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
