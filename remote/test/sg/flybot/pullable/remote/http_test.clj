(ns sg.flybot.pullable.remote.http-test
  "Tests for HTTP transport implementation"
  (:require
   [clojure.test :refer [deftest is testing]]
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

(def test-api
  (fn [_req]
    {:data {:user {:name "Alice" :age 30}
            :config {:debug true}}
     :schema {:user {:name :string :age :number}
              :config {:debug :boolean}}}))

(def test-handler (http/make-handler test-api))

(defn pull-request [pattern]
  {:request-method :post
   :uri "/api"
   :headers {"content-type" "application/transit+json"
             "accept" "application/transit+json"}
   :body (http/encode {:pattern pattern} :transit-json)})

(deftest successful-pull-test
  (testing "POST /api with valid pattern returns 200"
    (let [resp (test-handler (pull-request '{:user {:name ?n}}))]
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
