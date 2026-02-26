(ns sg.flybot.flybot-site.system-test
  "Integration tests for flybot-site system.

   Goals:
   - All system components initialize correctly
   - API endpoints work (schema, CRUD operations)
   - Auth/role-based access works (collection-based)
   - Upload handler works (local mode)

   Uses in-memory Datahike and local uploads for isolation."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures] :as t]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clj-http.client :as http]
   [sg.flybot.flybot-site.server.system :as system]
   [sg.flybot.flybot-site.server.system.db :as db]
   [sg.flybot.pullable.collection :as coll]
   [robertluo.fun-map :refer [halt! touch]]))

;;=============================================================================
;; Test Config
;;=============================================================================

(def test-port 18765)

(def test-config
  {:mode :dev
   :server {:port test-port
            :base-url (str "http://localhost:" test-port)}
   :db {:backend :mem
        :id "test-blog"}
   :auth {:owner-emails #{"owner@test.com"}
          :google-client-id nil
          :google-client-secret nil}
   :session {:timeout 3600}
   :init {:seed? false}
   :uploads {:type :local
             :dir "/tmp/flybot-test-uploads"}
   :log {:publishers [{:type :console}]
         :context {:app "flybot-test"}}
   ;; Auto-login as owner for CRUD tests (dev mode grants all roles)
   :dev {:user {:id "owner"
                :email "owner@test.com"
                :name "Test Owner"}}})

;;=============================================================================
;; HTTP Helpers
;;=============================================================================

(defn api-request
  "Make API request with pattern query (POST with edn)."
  ([pattern] (api-request pattern nil))
  ([pattern session]
   (let [opts (cond-> {:url (str "http://localhost:" test-port "/api")
                       :method :post
                       :headers {"Content-Type" "application/edn"
                                 "Accept" "application/edn"}
                       :body (pr-str {:pattern pattern})
                       :as :string
                       :throw-exceptions false}
                session (assoc :cookies {"ring-session" {:value session}}))]
     (try
       (let [{:keys [status body]} (http/request opts)
             parsed (when (and body (not (str/blank? body)))
                      (edn/read-string body))]
         {:status status :body parsed})
       (catch Exception e
         {:status 500 :error (ex-message e)})))))

(defn schema-request
  "Fetch API schema."
  []
  (try
    (let [{:keys [status body]} (http/request
                                 {:url (str "http://localhost:" test-port "/api/_schema")
                                  :method :get
                                  :accept :edn
                                  :throw-exceptions false})]
      {:status status
       :body (when body (edn/read-string body))})
    (catch Exception e
      {:status 500 :error (ex-message e)})))

;;=============================================================================
;; Test System
;;=============================================================================

(def ^:dynamic *sys* nil)

(defn with-system [f]
  (let [sys (system/make-system test-config)]
    (try
      (touch sys) ; Start all components
      (Thread/sleep 500) ; Wait for server to be ready
      ;; Note: dev-user is auto-created by make-dev-user-component with all roles
      ;; Create additional test user for member-level tests
      (let [conn (:conn (::system/db sys))]
        (db/create-user! conn #:user{:id "tester"
                                     :email "tester@test.com"
                                     :name "Test User"
                                     :picture ""})
        (db/grant-role! conn "tester" :member))
      (binding [*sys* sys]
        (f))
      (finally
        (halt! sys)))))

(use-fixtures :once with-system)

;;=============================================================================
;; Tests
;;=============================================================================

(deftest system-startup-test
  (testing "System components initialize"
    (is (some? (::system/http-server *sys*)) "HTTP server started")
    (is (some? (::system/db *sys*)) "Database connected")
    (is (some? (::system/api-fn *sys*)) "API function created")
    (is (some? (::system/upload-handler *sys*)) "Upload handler created")))

(deftest api-schema-test
  (testing "Schema endpoint returns valid schema"
    (let [{:keys [status body]} (schema-request)]
      (is (= 200 status))
      (is (contains? body :schema) "Response has :schema")
      (is (contains? body :sample) "Response has :sample data"))))

(deftest posts-crud-test
  (testing "Create post via :member :posts collection"
    ;; Dev mode has all roles, so :member :posts is available
    ;; CRUD via pattern: {nil data} = create
    (let [new-post {:post/title "Test Post"
                    :post/content "---\ntags:\n  - test\n---\n\n# Hello\n\nThis is test content."
                    :post/tags ["test"]}
          {:keys [status body]} (api-request {:member {:posts {nil new-post}}})]
      (is (= 200 status))
      (is (map? body) "Returns a bindings map")))

  (testing "List posts"
    ;; :guest :posts is read-only, can list without role
    (let [{:keys [status body]} (api-request '{:guest {:posts ?all}})]
      (is (= 200 status))
      (is (seq (get body 'all)) "Posts list bound to all")))

  (testing "Read single post by ID"
    ;; :guest :posts supports reads
    (let [{body1 :body} (api-request '{:guest {:posts ?all}})
          post-id (-> body1 (get 'all) first :post/id)
          {:keys [status body]} (api-request {:guest {:posts {{:post/id post-id} '?post}}})]
      (is (= 200 status))
      (is (= "Test Post" (:post/title (get body 'post))))))

  (testing "Update post via :member :posts"
    (let [{body1 :body} (api-request '{:guest {:posts ?all}})
          post-id (-> body1 (get 'all) first :post/id)
          {:keys [status]} (api-request
                            {:member {:posts {{:post/id post-id}
                                              {:post/title "Updated Title"}}}})]
      (is (= 200 status))
      ;; Verify update via guest read
      (let [{:keys [body]} (api-request {:guest {:posts {{:post/id post-id} '?post}}})]
        (is (= "Updated Title" (:post/title (get body 'post)))))))

  (testing "Delete post via :member :posts"
    (let [{body1 :body} (api-request '{:guest {:posts ?all}})
          post-id (-> body1 (get 'all) first :post/id)
          initial-count (count (get body1 'all))
          {:keys [status]} (api-request {:member {:posts {{:post/id post-id} nil}}})]
      (is (= 200 status))
      ;; Verify deletion via guest read
      (let [{:keys [body]} (api-request '{:guest {:posts ?all}})]
        (is (= (dec initial-count) (count (get body 'all))))))))

(deftest post-history-test
  (testing "Post history tracks changes"
    ;; Create a post via :member :posts - the created post is returned in bindings
    (let [{:keys [body]} (api-request {:member {:posts {nil {:post/title "History Test"
                                                             :post/content "---\ntags:\n  - test\n---\n\nv1"}}}})
          ;; Extract post-id from the create response (mutation result bound to 'posts symbol)
          created-post (get body 'posts)
          post-id (:post/id created-post)]
      (is (some? post-id) "Post was created with an ID")
      ;; Update it twice via :member :posts
      (api-request {:member {:posts {{:post/id post-id} {:post/content "---\ntags:\n  - test\n---\n\nv2"}}}})
      (api-request {:member {:posts {{:post/id post-id} {:post/content "---\ntags:\n  - test\n---\n\nv3"}}}})
      ;; Check history via :member :posts/history (history requires member role)
      (let [{:keys [status body]} (api-request
                                   {:member {:posts/history {{:post/id post-id} '?versions}}})]
        (is (= 200 status))
        (is (= (count (get body 'versions)) 2)
            "History shows previous versions, not current")))))

(deftest collection-based-api-test
  (testing "Guest has :guest with :posts (read-only), other roles empty"
    (let [api-fn (::system/api-fn *sys*)
          {:keys [data]} (api-fn {})]  ; No session = guest
      (is (some? (:guest data)) "Guest has :guest")
      (is (some? (get-in data [:guest :posts])) "Guest has :guest :posts")
      (is (= {} (:member data)) "Guest has empty :member")
      (is (= {} (:admin data)) "Guest has empty :admin")
      (is (= {} (:owner data)) "Guest has empty :owner")))

  (testing "Member has :member with :posts and :me"
    (let [api-fn (::system/api-fn *sys*)
          {:keys [data]} (api-fn {:session {:user-id "tester"
                                            :user-email "tester@test.com"
                                            :user-name "Test User"
                                            :roles #{:member}}})]
      (is (some? (:member data)) "Member has :member")
      (is (some? (get-in data [:member :posts])) "Member has :member :posts")
      (is (some? (get-in data [:member :me])) "Member has :member :me")
      (is (= "tester@test.com" (get-in data [:member :me :email])))))

  (testing "Admin has :admin with :posts"
    (let [api-fn (::system/api-fn *sys*)
          {:keys [data]} (api-fn {:session {:user-id "owner"
                                            :user-email "owner@test.com"
                                            :roles #{:member :admin}}})]
      (is (some? (:admin data)) "Admin has :admin")
      (is (some? (get-in data [:admin :posts])) "Admin has :admin :posts")))

  (testing "Owner has :owner with :users"
    (let [api-fn (::system/api-fn *sys*)
          {:keys [data]} (api-fn {:session {:user-id "owner"
                                            :user-email "owner@test.com"
                                            :roles #{:member :admin :owner}}})]
      (is (some? (:owner data)) "Owner has :owner")
      (is (some? (get-in data [:owner :users])) "Owner has :owner :users")
      (is (seq (seq (get-in data [:owner :users]))) "Users collection has data")))

  (testing "Member can create post via :member :posts collection"
    (let [api-fn (::system/api-fn *sys*)
          {:keys [data]} (api-fn {:session {:user-id "tester"
                                            :user-email "tester@test.com"
                                            :roles #{:member}}})
          result (coll/mutate! (get-in data [:member :posts]) nil
                               {:post/title "Collection Test"
                                :post/content "test"
                                :post/tags []})]
      (is (= "Collection Test" (:post/title result)))
      (is (= "tester@test.com" (get-in result [:post/author :user/email])) "Author auto-set")))

  (testing "Ownership enforcement - member cannot update other's post"
    ;; First, create a post as owner
    (let [api-fn (::system/api-fn *sys*)
          {:keys [data]} (api-fn {:session {:user-id "owner"
                                            :user-email "owner@test.com"
                                            :roles #{:member}}})
          owner-post (coll/mutate! (get-in data [:member :posts]) nil
                                   {:post/title "Owner's Post"
                                    :post/content "test"
                                    :post/tags []})
          owner-post-id (:post/id owner-post)]
      ;; Now try to update it as tester (different user) - returns error
      (let [{data2 :data} (api-fn {:session {:user-id "tester"
                                             :user-email "tester@test.com"
                                             :roles #{:member}}})
            result (coll/mutate! (get-in data2 [:member :posts])
                                 {:post/id owner-post-id}
                                 {:post/title "Hacked!"})]
        (is (= :forbidden (:type (:error result))) "Cannot update other's post"))))

  (testing "Admin can update any post via :admin :posts"
    ;; Get the owner's post we just created
    (let [api-fn (::system/api-fn *sys*)
          {:keys [data]} (api-fn {:session {:user-id "tester"
                                            :user-email "tester@test.com"
                                            :roles #{:member :admin}}})
          posts (seq (get-in data [:member :posts]))
          owner-post (first (filter #(= "Owner's Post" (:post/title %)) posts))
          owner-post-id (:post/id owner-post)
          result (coll/mutate! (get-in data [:admin :posts])
                               {:post/id owner-post-id}
                               {:post/title "Admin Override"})]
      (is (= "Admin Override" (:post/title result)) "Admin can update any post"))))

(deftest upload-handler-test
  (testing "Local upload handler works"
    (let [upload-handler (::system/upload-handler *sys*)
          temp-file (java.io.File/createTempFile "test" ".png")]
      (try
        (spit temp-file "fake image data")
        (let [{:keys [url]} (upload-handler temp-file "test.png")]
          (is (string? url))
          (is (re-matches #"/uploads/.*\.png" url)))
        (finally
          (.delete temp-file))))))

(deftest error-handling-test
  (testing "Invalid pattern returns schema-violation 403"
    (let [{:keys [status]} (api-request '{:invalid-key ?x})]
      (is (= 403 status)))))

;;=============================================================================
;; REPL helpers
;;=============================================================================

(comment
  ;; Run tests from REPL
  (t/run-tests 'sg.flybot.flybot-site.system-test)

  ;; Run single test
  (with-system #(system-startup-test))
  (with-system #(posts-crud-test))
  (with-system #(collection-based-api-test)))
