(ns sg.flybot.pullable.rct-test
  "Wrapper to run RCT tests alongside clojure.test"
  (:require
   [clojure.test :refer [deftest]]
   [com.mjdowney.rich-comment-tests.test-runner :as tr]))

(deftest rct-tests
  (tr/run-tests-in-file-tree! :dirs #{"src" "test"}))
