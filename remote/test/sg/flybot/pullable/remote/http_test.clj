(ns sg.flybot.pullable.remote.http-test
  "Tests for HTTP transport implementation"
  (:require
   [clojure.test :refer [deftest is testing]]
   [sg.flybot.pullable.collection :as coll]
   [sg.flybot.pullable.remote.http :as http]))

;;=============================================================================
;; Encoding Tests
;;=============================================================================

(deftest transit-json-roundtrip-test
  (testing "transit-json encoding/decoding"
    (let [data {:pattern '{:x ?x} :params {:a 1}}
          result (-> data (http/encode :transit-json) (http/decode :transit-json))]
      (is (= {:x '?x} (:pattern result)))
      (is (= {:a 1} (:params result))))))

(deftest transit-msgpack-roundtrip-test
  (testing "transit-msgpack encoding/decoding"
    (let [data {:data {:name "Alice"} :vars {'n "Alice"}}
          result (-> data (http/encode :transit-msgpack) (http/decode :transit-msgpack))]
      (is (= {:name "Alice"} (:data result)))
      (is (= "Alice" (get (:vars result) 'n))))))

(deftest edn-roundtrip-test
  (testing "edn encoding/decoding"
    (let [data {:pattern '{:user {:name ?n}}}
          result (-> data (http/encode :edn) (http/decode :edn))]
      (is (= '{:user {:name ?n}} (:pattern result))))))

;;=============================================================================
;; Handler Tests
;;=============================================================================

;; Single api-fn exercises: reads, mutations, error detection, schema.
;; - :public     → read-only collection
;; - :guarded    → wrap-mutable with ownership check (update returns :error)
;; - :private    → role-gated error branch (like with-role returning {:error ...})
;; - :restricted → has schema (only :name key declared)

(def test-api
  (let [src (coll/atom-source {:initial [{:id 1 :name "Alice"} {:id 2 :name "Bob"}]})
        items (coll/collection src)
        guarded (coll/wrap-mutable items
                                   (fn [coll query value]
                                     (if (some? query)
                                       {:error {:type :forbidden :message "Not yours"}}
                                       (coll/mutate! coll query value))))]
    (fn [_req]
      {:data   {:public     {:items (coll/read-only items)}
                :guarded    {:items guarded}
                :private    {:error {:type :forbidden :message "Not authorized"}}
                :restricted {:name "Alice" :secret "s3cret"}}
       :schema {:public :any :guarded :any :private :any
                :restricted {:name :string}}
       :errors {:detect :error
                :codes  {:forbidden 403}}})))

(def test-handler (http/make-handler test-api))

(defn pull-request [pattern]
  {:request-method :post
   :uri "/api"
   :headers {"content-type" "application/transit+json"
             "accept" "application/transit+json"}
   :body (http/encode {:pattern pattern} :transit-json)})

;;--- routing ---

(deftest successful-pull-test
  (testing "POST /api with valid pattern returns 200"
    (let [resp (test-handler (pull-request '{:public {:items ?all}}))]
      (is (= 200 (:status resp))))))

(deftest schema-introspection-test
  (testing "GET /api/_schema returns 200"
    (let [resp (test-handler {:request-method :get
                              :uri "/api/_schema"
                              :headers {"accept" "application/transit+json"}})]
      (is (= 200 (:status resp))))))

(deftest method-not-allowed-test
  (testing "GET /api returns 405"
    (let [resp (test-handler {:request-method :get
                              :uri "/api"
                              :headers {"accept" "application/transit+json"}})]
      (is (= 405 (:status resp))))))

(deftest not-found-test
  (testing "Unknown path returns 404"
    (let [resp (test-handler {:request-method :get
                              :uri "/other"
                              :headers {"accept" "application/transit+json"}})]
      (is (= 404 (:status resp))))))

(deftest content-negotiation-test
  (testing "Response respects Accept header"
    (is (= "application/transit+msgpack"
           (get-in (test-handler {:request-method :get
                                  :uri "/api/_schema"
                                  :headers {"accept" "application/transit+msgpack"}})
                   [:headers "Content-Type"])))
    (is (= "application/edn"
           (get-in (test-handler {:request-method :get
                                  :uri "/api/_schema"
                                  :headers {"accept" "application/edn"}})
                   [:headers "Content-Type"])))))

;;--- read errors ---

(deftest read-error-single-path-403-test
  (testing "Single error path returns HTTP 403"
    (let [resp (test-handler (pull-request '{:private {:items ?all}}))]
      (is (= 403 (:status resp))))))

(deftest read-error-partial-success-200-test
  (testing "Multi-path with one error returns HTTP 200 (partial success)"
    (let [resp (test-handler (pull-request '{:public {:items ?all} :private {:items ?s}}))]
      (is (= 200 (:status resp))))))

(deftest read-error-all-paths-error-403-test
  (testing "All paths with error returns HTTP 403"
    (let [all-error-api (fn [_req]
                          {:data   {:a {:error {:type :forbidden :message "No"}}
                                    :b {:error {:type :forbidden :message "No"}}}
                           :errors {:detect :error :codes {:forbidden 403}}})
          handler (http/make-handler all-error-api)
          resp (handler (pull-request '{:a {:x ?x} :b {:y ?y}}))]
      (is (= 403 (:status resp))))))

(deftest read-nested-error-403-test
  (testing "Nested error detected recursively returns 403"
    (let [api (fn [_req]
                {:data   {:section {:ok {:name "Alice"}
                                    :denied {:error {:type :forbidden :message "No"}}}}
                 :errors {:detect :error :codes {:forbidden 403}}})
          handler (http/make-handler api)
          resp (handler (pull-request '{:section {:denied {:name ?n}}}))]
      (is (= 403 (:status resp))))))

(deftest read-nested-error-partial-success-200-test
  (testing "Sibling of nested error returns 200 (partial success)"
    (let [api (fn [_req]
                {:data   {:section {:ok {:name "Alice"}
                                    :denied {:error {:type :forbidden :message "No"}}}}
                 :errors {:detect :error :codes {:forbidden 403}}})
          handler (http/make-handler api)
          resp (handler (pull-request '{:section {:ok {:name ?n}}}))]
      (is (= 200 (:status resp))))))

(deftest read-schema-violation-403-test
  (testing "Accessing undeclared schema key returns 403"
    (let [resp (test-handler (pull-request '{:restricted {:secret ?s}}))]
      (is (= 403 (:status resp))))))

(deftest read-failure-without-errors-config-422-test
  (testing "Read failure without :errors config returns 422 (generic match-failure)"
    (let [api (fn [_req] {:data {:broken nil}})
          handler (http/make-handler api)
          resp (handler (pull-request '{:broken {:deep ?v}}))]
      (is (= 422 (:status resp))))))

;;--- mutation errors ---

(deftest mutation-success-200-test
  (testing "Successful mutation returns 200"
    (let [resp (test-handler (pull-request '{:guarded {:items {nil {:name "Carol"}}}}))]
      (is (= 200 (:status resp))))))

(deftest mutation-error-detected-403-test
  (testing "Mutation error from wrap-mutable detected via :detect returns 403"
    (let [resp (test-handler (pull-request '{:guarded {:items {{:id 1} {:name "Bob"}}}}))]
      (is (= 403 (:status resp))))))

(deftest mutation-role-gated-forbidden-test
  (testing "Mutation through role-gated error map returns 403 (forbidden detected before collection)"
    (let [resp (test-handler (pull-request '{:private {:items {nil {:name "X"}}}}))]
      (is (= 403 (:status resp))))))

;;--- exceptions ---

(deftest mutation-exception-500-test
  (testing "Exception during mutation returns 500"
    (let [throwing-coll (reify
                          clojure.lang.ILookup
                          (valAt [_ _] nil)
                          (valAt [_ _ nf] nf)
                          coll/Mutable
                          (mutate! [_ _ _]
                            (throw (ex-info "DB connection lost" {}))))
          api (fn [_req] {:data {:items throwing-coll}})
          handler (http/make-handler api)
          resp (handler (pull-request '{:items {nil {:name "X"}}}))]
      (is (= 500 (:status resp))))))

(deftest read-exception-500-test
  (testing "Exception from api-fn on read returns 500"
    (let [api (fn [_req] (throw (ex-info "Service unavailable" {})))
          handler (http/make-handler api)
          resp (handler (pull-request '{:x ?x}))]
      (is (= 500 (:status resp))))))

(deftest mutation-exception-from-api-fn-500-test
  (testing "Exception from api-fn on mutation returns 500"
    (let [api (fn [_req] (throw (ex-info "Service unavailable" {})))
          handler (http/make-handler api)
          resp (handler (pull-request '{:x {nil {:a 1}}}))]
      (is (= 500 (:status resp))))))
