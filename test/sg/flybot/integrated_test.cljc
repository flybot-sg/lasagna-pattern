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
      '{:a ?a :b {:c ?c}} {:a 1 :b {:c 2 :d 3} :e 4 } '{a 1 c 2 & {:a 1 :b {:c 2 :d 3} :e 4}}))
  (testing "placeholder"
    (are [pattern data expected] (= expected ((sut/ptn-fn pattern) data))
      '[_ 2 _ ?a] [1 2 3 4] '{a 4 & [1 2 3 4]}))
  (testing "terminal matcher"
    (are [pattern data expected] (= expected ((sut/ptn-fn pattern) data))
      '[1 2 $] [1 2 3 4] nil
      [1 '?a 3 '$] [1 2 3] '{a 2 & [1 2 3]}))
  (testing ":to option"
    (are [pattern data expected] (= expected ((sut/ptn-fn pattern) data))
      '[1 (!a :to 5)] [1 2 3 4] '{a 5 & [1 5 3 4]}
      '[1 (!_ :to ::sut/remove)] [1 2 3 4] '{& [1 3 4]}
      ))
  (testing ":edit option"
    (are [pattern data expected] (= expected ((sut/ptn-fn pattern) data))
      [1 (list '!a :edit inc)] [1 2 3 4] '{a 3 & [1 3 3 4]}))
  (testing ":with option"
    (are [pattern data expected] (= expected ((sut/ptn-fn pattern) data))
      [1 '(!_ :with [5])] [1 #(* 2 %) 3] '{& [1 10 3]}))
  (testing "complex patterns"
    (are [pattern data expected] (= expected ((sut/ptn-fn pattern) data)) 
      ['_ '_ (list '?a :when odd?) {:b {:c ['?a '_ '$]}}]
      [1 2 3 {:b {:c [3 5]}}]
      '{a 3 & [1 2 3 {:b {:c [3 5]}}]}
      )))