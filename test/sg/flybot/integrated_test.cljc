(ns sg.flybot.integrated-test
  (:require
   [clojure.test :refer [deftest testing are]]
   [sg.flybot.pullable :as sut]
   [com.mjdowney.rich-comment-tests.test-runner :as tr]))

(deftest rct-tests
  (tr/run-tests-in-file-tree! :dirs #{"src"}))

(deftest test-overall
  (testing "basic value matching for '&"
    (are [pattern data expected] (= expected ((sut/ptn-fn pattern) data))
      '[1 2] [1 2] {'& [1 2]}
      '[1 2] [1 2 3] nil
      '[[2 3] 1] '[[2 3 4] 1] nil
      '{:a 1 :b 2} {:a 1 :b 2} {'& {:a 1 :b 2}}))
  (testing "logical variable binding"
    (are [pattern data expected] (= expected (dissoc ((sut/ptn-fn pattern) data) '&))
      '[1 ?a 3] [1 2 3] '{a 2}
      '[1 {:a {:b ?b}}] [1 {:a {:b 2}}] '{b 2}))
  (testing "unification"
    (are [pattern data expected] (= expected (dissoc ((sut/ptn-fn pattern) data) '&))
      '[1 ?a ?a] [1 2 2] '{a 2}
      '[1 ?a ?a] [1 2 3] nil
      '{:a ?a :b {:c ?a}} {:a 1 :b {:c 1}} '{a 1} 
      '{:a ?a :b {:c ?a}} {:a 1 :b {:c 2}} nil
      ))
  (testing ":when option"
    (are [pattern data expected] (= expected (dissoc ((sut/ptn-fn pattern) data) '&))
      [1 (list '?a :when even?)] [1 2] '{a 2}
      [1 (list '?a :when even?)] [1 3] nil
      ))
  (testing "map match preserve unmatched entries"
    (are [pattern data expected] (= expected ((sut/ptn-fn pattern) data))
      '{:a ?a} {:a 1 :b 2 :c 3} '{a 1 & {:b 2 :c 3}})))