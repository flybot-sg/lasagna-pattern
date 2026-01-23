(ns sg.flybot.pullable.remote.rct-test
  "Wrapper to run RCT tests alongside clojure.test"
  (:require
   [clojure.test :refer [deftest]]
   [com.mjdowney.rich-comment-tests.test-runner :as rct]))

(deftest rct-tests
  (rct/run-tests-in-file-tree! :dirs #{"src"}))
