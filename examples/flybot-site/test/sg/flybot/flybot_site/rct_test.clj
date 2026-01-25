(ns sg.flybot.flybot-site.rct-test
  "Integration of Rich Comment Tests with clojure.test.

   Runs all RCT tests in src/ so Kaocha picks them up."
  (:require
   [clojure.test :refer [deftest]]
   [com.mjdowney.rich-comment-tests.test-runner :as rct]))

(deftest rct-tests
  (rct/run-tests-in-file-tree! :dirs #{"src" "test"}))
