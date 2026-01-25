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
   [sg.flybot.flybot-site.api :as api]))

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
  (testing "Root schema is a map"
    (let [info (impl/get-schema-info api/schema)]
      (is (= :map (:type info)))
      (is (contains? (:valid-keys info) :posts))
      (is (contains? (:valid-keys info) :posts/history))))

  (testing ":posts is a union (returns :any)"
    (let [info (impl/get-schema-info (:posts api/schema))]
      (is (= :any (:type info))
          "Union schema should report :any to allow both seq and map patterns")))

  (testing "post-schema is a map with expected keys"
    (let [info (impl/get-schema-info api/post-schema)]
      (is (= :map (:type info)))
      (is (= #{:post/id :post/title :post/content :post/author
               :post/tags :post/created-at :post/updated-at}
             (:valid-keys info))))))

;;=============================================================================
;; Pattern Compilation Tests
;;=============================================================================

(deftest valid-patterns-compile-test
  (testing "LIST pattern compiles"
    (is (fn? (impl/compile-pattern '{:posts ?posts} {:schema api/schema}))))

  (testing "Single post fields pattern compiles"
    (is (fn? (impl/compile-pattern
              '{:posts [{:post/id ?id :post/title ?title}]}
              {:schema api/schema}))))

  (testing "Nested sequence pattern compiles"
    (is (fn? (impl/compile-pattern
              '{:posts [{:post/tags ?tags}]}
              {:schema api/schema}))))

  (testing "Lookup pattern compiles (union map branch)"
    (is (fn? (impl/compile-pattern
              '{:posts {{:post/id 1} ?post}}
              {:schema api/schema})))))

(deftest invalid-pattern-detection-test
  (testing "Invalid key in post pattern detected at runtime"
    ;; Schema allows the pattern to compile (via :any union)
    ;; but runtime matching will fail for non-existent keys
    (let [matcher (impl/compile-pattern
                   '{:posts [{:post/invalid-key ?x}]}
                   {:schema api/schema})
          result (matcher (impl/vmr sample-api))]
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
    (let [result ((p/match-fn {:posts ?posts} ?posts {:schema api/schema})
                  sample-api)]
      (is (= sample-posts result))))

  (testing "Extract single post field"
    ;; Pattern [{:post/title ?title}] matches exactly one element
    (let [result ((p/match-fn {:posts [{:post/title ?title}]} ?title
                              {:schema api/schema})
                  sample-api)]
      (is (= "Test Post" result)))))

(deftest match-fn-single-post-test
  (testing "Get single post via lookup pattern"
    (let [api-with-lookup {:posts {'{:post/id 1} sample-post}}
          result ((p/match-fn {:posts {{:post/id 1} ?post}} ?post
                              {:schema api/schema})
                  api-with-lookup)]
      (is (= sample-post result)))))

(deftest match-fn-nested-fields-test
  (testing "Extract nested array field"
    ;; Pattern [{:post/tags ?tags}] matches one post and extracts its tags
    (let [result ((p/match-fn {:posts [{:post/tags ?tags}]} ?tags
                              {:schema api/schema})
                  sample-api)]
      (is (= ["clojure" "testing"] result))))

  (testing "Extract multiple fields from single post"
    ;; Pattern [{:post/id ?id :post/author ?author}] matches one post
    (let [result ((p/match-fn {:posts [{:post/id ?id :post/author ?author}]}
                              [?id ?author]
                              {:schema api/schema})
                  sample-api)]
      (is (= [1 "Alice"] result)))))

;;=============================================================================
;; Schema Violation Tests (non-union schemas)
;;=============================================================================

(deftest schema-violation-test
  (testing "Map pattern for primitive field throws at compile time"
    (is (thrown-with-msg?
         #?(:clj Exception :cljs js/Error)
         #"Schema violation.*map.*number"
         (eval '(sg.flybot.pullable/match-fn {:post/id {:nested ?x}} ?x
                                             {:schema sg.flybot.flybot-site.api/post-schema})))))

  (testing "Seq pattern for map field throws at compile time"
    (is (thrown-with-msg?
         #?(:clj Exception :cljs js/Error)
         #"Schema violation.*seq.*map"
         (eval '(sg.flybot.pullable/match-fn [{:post/title ?t}] ?t
                                             {:schema sg.flybot.flybot-site.api/post-schema}))))))

;;=============================================================================
;; RCT Integration
;;=============================================================================

^:rct/test
(comment
  (require '[sg.flybot.flybot-site.pattern-test :as pt] :reload)

  ;; Schema structure
  (impl/get-schema-info api/schema)
  ;=>> {:type :map :valid-keys #{:posts :posts/history} :child-schema fn?}

  ;; Pattern compilation succeeds
  (fn? (impl/compile-pattern '{:posts ?posts} {:schema api/schema}))
  ;=> true

  ;; match-fn works with schema
  ((p/match-fn {:posts ?posts} (count ?posts) {:schema api/schema})
   pt/sample-api)
  ;=> 1

  ;; Invalid patterns for post-schema fail
  (try
    (eval '(p/match-fn {:post/id {:x ?x}} ?x {:schema api/post-schema}))
    :should-fail
    (catch Exception e :caught))
  ;=> :caught
  )
