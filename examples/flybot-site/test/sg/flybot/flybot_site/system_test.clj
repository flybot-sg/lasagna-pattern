(ns sg.flybot.flybot-site.system-test
  "Integration tests for flybot-site system.

   Goals:
   - All system components initialize correctly
   - API endpoints work (schema, CRUD operations)
   - Auth/role-based access works
   - Upload handler works (local mode)

   Uses in-memory Datahike and local uploads for isolation."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures] :as t]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clj-http.client :as http]
   [sg.flybot.flybot-site.system :as system]
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
         :context {:app "flybot-test"}}})

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
  (testing "Create post"
    (let [new-post {:post/title "Test Post"
                    :post/content "---\nauthor: tester\ntags:\n  - test\n---\n\n# Hello\n\nThis is test content."
                    :post/author "tester"
                    :post/tags ["test"]}
          {:keys [status body]} (api-request {:posts {nil new-post}})]
      (is (= 200 status))
      ;; Create returns the created post bound to the nil key pattern
      (is (map? body) "Returns a bindings map")))

  (testing "List posts"
    (let [{:keys [status body]} (api-request '{:posts ?all})]
      (is (= 200 status))
      (is (seq (get body 'all)) "Posts list bound to all")))

  (testing "Read single post by ID"
    ;; First get the ID from list
    (let [{body1 :body} (api-request '{:posts ?all})
          post-id (-> body1 (get 'all) first :post/id)
          {:keys [status body]} (api-request {:posts {{:post/id post-id} '?post}})]
      (is (= 200 status))
      (is (= "Test Post" (:post/title (get body 'post))))))

  (testing "Update post"
    (let [{body1 :body} (api-request '{:posts ?all})
          post-id (-> body1 (get 'all) first :post/id)
          {:keys [status]} (api-request
                            {:posts {{:post/id post-id}
                                     {:post/title "Updated Title"}}})]
      (is (= 200 status))
      ;; Verify update
      (let [{:keys [body]} (api-request {:posts {{:post/id post-id} '?post}})]
        (is (= "Updated Title" (:post/title (get body 'post)))))))

  (testing "Delete post"
    (let [{body1 :body} (api-request '{:posts ?all})
          post-id (-> body1 (get 'all) first :post/id)
          initial-count (count (get body1 'all))
          {:keys [status]} (api-request {:posts {{:post/id post-id} nil}})]
      (is (= 200 status))
      ;; Verify deletion
      (let [{:keys [body]} (api-request '{:posts ?all})]
        (is (= (dec initial-count) (count (get body 'all))))))))

(deftest post-history-test
  (testing "Post history tracks changes"
    ;; Create a post - need frontmatter for author/tags extraction
    (let [{body1 :body} (api-request {:posts {nil {:post/title "History Test"
                                                   :post/content "---\nauthor: tester\ntags:\n  - test\n---\n\nv1"}}})
          ;; Get the created post's ID from list
          {body2 :body} (api-request '{:posts ?all})
          post-id (-> body2 (get 'all) last :post/id)]
      ;; Update it twice
      (api-request {:posts {{:post/id post-id} {:post/content "---\nauthor: tester\ntags:\n  - test\n---\n\nv2"}}})
      (api-request {:posts {{:post/id post-id} {:post/content "---\nauthor: tester\ntags:\n  - test\n---\n\nv3"}}})
      ;; Check history
      (let [{:keys [status body]} (api-request
                                   {:posts/history {{:post/id post-id} '?versions}})]
        (is (= 200 status))
        (is (>= (count (get body 'versions)) 3)
            "History contains multiple versions")))))

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
  (testing "Invalid pattern returns error gracefully"
    (let [{:keys [status]} (api-request '{:invalid-key ?x})]
      ;; Should not crash the server
      (is (number? status)))))

;;=============================================================================
;; REPL helpers
;;=============================================================================

(comment
  ;; Run tests from REPL
  (t/run-tests 'sg.flybot.flybot-site.system-test)

  ;; Run single test
  (with-system #(system-startup-test))
  (with-system #(posts-crud-test)))
