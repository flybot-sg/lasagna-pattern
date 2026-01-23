(ns sg.flybot.blog.rct-test
  "RCT runner for blog component."
  (:require
   [clojure.test :refer [deftest]]
   [com.mjdowney.rich-comment-tests.test-runner :as rct]))

(deftest rct-tests
  (rct/run-tests-in-file-tree! :dirs #{"src"}))
