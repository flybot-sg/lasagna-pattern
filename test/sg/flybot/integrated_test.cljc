(ns sg.flybot.integrated-test
  (:require
   [clojure.test :refer [deftest testing are]]
   [com.mjdowney.rich-comment-tests.test-runner :as tr]))

(deftest rct-tests
  (tr/run-tests-in-file-tree! :dirs #{"src"}))
