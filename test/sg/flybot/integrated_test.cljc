(ns sg.flybot.integrated-test
  (:require
   [clojure.test :refer [deftest testing are]]
   [sg.flybot.pullable :as sut]
   [com.mjdowney.rich-comment-tests.test-runner :as tr]))

(deftest rct-tests
  (tr/run-tests-in-file-tree! :dirs #{"src"}))

(deftest test-overall
  (testing "basic patterns"
    (are [pattern data expected] (= expected ((sut/ptn-fn pattern) data))
      '[1 2] [1 2] {'& [1 2]}
      '[1 2] [1 2 3] nil
      '[[2 3] 1] '[[2 3 4] 1] nil)))
