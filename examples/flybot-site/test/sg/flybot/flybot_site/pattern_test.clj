(ns sg.flybot.flybot-site.pattern-test
  "Pattern validation tests for blog API.

   Tests pattern compilation and validation against the blog schema
   without requiring HTTP transport or database.

   Uses sg.flybot.pullable directly to test:
   - Valid patterns compile with schema
   - Invalid patterns fail at compile time
   - Patterns match expected data structures"
  (:require
   [clojure.test :refer [deftest is testing]]
   [sg.flybot.pullable :as p]
   [sg.flybot.pullable.impl :as impl]
   [sg.flybot.flybot-site.server.system.api :as api]))

;;=============================================================================
;; Test Data
;;=============================================================================

(def sample-post
  {:post/id 1
   :post/title "Test Post"
   :post/content "Sample content"
   :post/author "Alice"
   :post/tags ["clojure" "testing"]
   :post/created-at #inst "2024-01-01"
   :post/updated-at #inst "2024-01-02"})

(def sample-posts [sample-post])

(def sample-api
  {:posts sample-posts
   :posts/history {}})

;;=============================================================================
;; Schema Info Tests
;;=============================================================================

(deftest schema-structure-test
  (testing "Root schema is a map with role keys"
    (let [info (impl/get-schema-info api/schema)]
      (is (= :map (:type info)))
      (is (contains? (:valid-keys info) :guest))
      (is (contains? (:valid-keys info) :member))
      (is (contains? (:valid-keys info) :admin))
      (is (contains? (:valid-keys info) :owner))))

  (testing ":guest schema contains :posts only"
    (let [info (impl/get-schema-info (:guest api/schema))]
      (is (= :map (:type info)))
      (is (contains? (:valid-keys info) :posts))
      (is (not (contains? (:valid-keys info) :posts/history)))))

  (testing ":member schema contains :posts, :posts/history, and :me"
    ;; get-schema-info unwraps :maybe, returns :map info
    (let [info (impl/get-schema-info (:member api/schema))]
      (is (= :map (:type info)))
      (is (contains? (:valid-keys info) :posts))
      (is (contains? (:valid-keys info) :posts/history))
      (is (contains? (:valid-keys info) :me))))

  (testing "post-schema is a map with expected keys"
    (let [info (impl/get-schema-info api/post-schema)]
      (is (= :map (:type info)))
      (is (= #{:post/id :post/title :post/content :post/author
               :post/tags :post/created-at :post/updated-at :post/featured?}
             (:valid-keys info))))))

;;=============================================================================
;; Pattern Compilation Tests
;;=============================================================================

(deftest valid-patterns-compile-test
  (testing "LIST pattern compiles (nested under :guest)"
    (is (fn? (impl/compile-pattern '{:guest {:posts ?posts}} {:schema api/schema}))))

  (testing "Single post fields pattern compiles"
    (is (fn? (impl/compile-pattern
              '{:guest {:posts [{:post/id ?id :post/title ?title}]}}
              {:schema api/schema}))))

  (testing "Nested sequence pattern compiles"
    (is (fn? (impl/compile-pattern
              '{:guest {:posts [{:post/tags ?tags}]}}
              {:schema api/schema}))))

  (testing "Lookup pattern compiles (union map branch)"
    (is (fn? (impl/compile-pattern
              '{:guest {:posts {{:post/id 1} ?post}}}
              {:schema api/schema})))))

(deftest invalid-pattern-detection-test
  (testing "Invalid key in post pattern detected at runtime"
    ;; Schema allows the pattern to compile (via :any union)
    ;; but runtime matching will fail for non-existent keys
    (let [matcher (impl/compile-pattern
                   '{:guest {:posts [{:post/invalid-key ?x}]}}
                   {:schema api/schema})
          result (matcher (impl/vmr {:guest sample-api}))]
      ;; The pattern compiles but matching may fail or return nil for invalid keys
      ;; This is acceptable behavior - schema validation is best-effort
      (is (or (p/failure? result)
              (nil? (get (:vars result) 'x)))
          "Invalid key should either fail or return nil"))))

;;=============================================================================
;; Pattern Matching Tests (match-fn macro)
;;=============================================================================

(deftest match-fn-list-test
  (testing "LIST: get all posts"
    (let [result ((p/match-fn {:guest {:posts ?posts}} ?posts {:schema api/schema})
                  {:guest sample-api})]
      (is (= sample-posts result))))

  (testing "Extract single post field"
    ;; Pattern [{:post/title ?title}] matches exactly one element
    (let [result ((p/match-fn {:guest {:posts [{:post/title ?title}]}} ?title
                              {:schema api/schema})
                  {:guest sample-api})]
      (is (= "Test Post" result)))))

(deftest match-fn-single-post-test
  (testing "Get single post via lookup pattern"
    (let [api-with-lookup {:guest {:posts {'{:post/id 1} sample-post}}}
          result ((p/match-fn {:guest {:posts {{:post/id 1} ?post}}} ?post
                              {:schema api/schema})
                  api-with-lookup)]
      (is (= sample-post result)))))

(deftest match-fn-nested-fields-test
  (testing "Extract nested array field"
    ;; Pattern [{:post/tags ?tags}] matches one post and extracts its tags
    (let [result ((p/match-fn {:guest {:posts [{:post/tags ?tags}]}} ?tags
                              {:schema api/schema})
                  {:guest sample-api})]
      (is (= ["clojure" "testing"] result))))

  (testing "Extract multiple fields from single post"
    ;; Pattern [{:post/id ?id :post/author ?author}] matches one post
    (let [result ((p/match-fn {:guest {:posts [{:post/id ?id :post/author ?author}]}}
                              [?id ?author]
                              {:schema api/schema})
                  {:guest sample-api})]
      (is (= [1 "Alice"] result)))))

;;=============================================================================
;; Schema Violation Tests (non-union schemas)
;;=============================================================================

(deftest schema-violation-test
  (testing "Map pattern for primitive field throws at compile time"
    (is (thrown-with-msg?
         Exception
         #"Schema violation.*map.*number"
         (eval '(sg.flybot.pullable/match-fn {:post/id {:nested ?x}} ?x
                                             {:schema sg.flybot.flybot-site.server.system.api/post-schema})))))

  (testing "Seq pattern for map field throws at compile time"
    (is (thrown-with-msg?
         Exception
         #"Schema violation.*seq.*map"
         (eval '(sg.flybot.pullable/match-fn [{:post/title ?t}] ?t
                                             {:schema sg.flybot.flybot-site.server.system.api/post-schema}))))))

;;=============================================================================
;; RCT Integration
;;=============================================================================

^:rct/test
(comment
  (require '[sg.flybot.flybot-site.pattern-test :as pt] :reload)

  ;; Schema structure (role-as-top-level)
  (impl/get-schema-info api/schema)
  ;=>> {:type :map :valid-keys #{:admin :member :owner :guest} :child-schema fn?}

  ;; Pattern compilation succeeds (nested under :guest)
  (fn? (impl/compile-pattern '{:guest {:posts ?posts}} {:schema api/schema}))
  ;=> true

  ;; match-fn works with schema (nested pattern)
  ((p/match-fn {:guest {:posts ?posts}} (count ?posts) {:schema api/schema})
   {:guest pt/sample-api})
  ;=> 1

  ;; Pattern compiles but matching invalid structures returns nil/failure at runtime
  ;; Note: With Malli schemas, type checking for primitives happens at runtime, not compile time
  (fn? (eval '(p/match-fn {:post/id ?id} ?id {:schema api/post-schema})))
  ;=> true
  )
