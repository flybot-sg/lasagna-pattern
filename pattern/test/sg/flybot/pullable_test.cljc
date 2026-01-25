(ns sg.flybot.pullable-test
  "ClojureScript-compatible tests for the pullable pattern matching library.
   Uses standard clojure.test instead of RCT for cross-platform compatibility."
  #?(:clj
     (:require
      [clojure.test :refer [deftest is testing]]
      [sg.flybot.pullable :as p :refer [match-fn rule apply-rules failure?]])
     :cljs
     (:require
      [clojure.test :refer [deftest is testing]]
      [sg.flybot.pullable :as p :refer [apply-rules failure?]]))
  #?(:cljs (:require-macros [sg.flybot.pullable :refer [match-fn rule]])))

;;=============================================================================
;; Basic Pattern Matching
;;=============================================================================

(deftest match-fn-basic-test
  (testing "simple variable binding"
    (is (= 1 ((match-fn ?x ?x) 1)))
    (is (= "hello" ((match-fn ?x ?x) "hello"))))

  (testing "wildcard matches anything"
    (is (= :matched ((match-fn ?_ :matched) 42)))
    (is (= :matched ((match-fn ?_ :matched) "anything"))))

  (testing "map pattern matching"
    (is (= 1 ((match-fn {:a ?a} ?a) {:a 1})))
    (is (= 3 ((match-fn {:a ?a :b ?b} (+ ?a ?b)) {:a 1 :b 2})))
    (is (failure? ((match-fn {:a ?a} ?a) "not a map"))))

  (testing "nested map patterns"
    (is (= "inner" ((match-fn {:outer {:inner ?x}} ?x)
                    {:outer {:inner "inner"}}))))

  (testing "$ binding to matched value"
    (is (= {:a 1 :b 2 :sum 1}
           ((match-fn {:a ?x} (assoc $ :sum ?x)) {:a 1 :b 2})))))

;;=============================================================================
;; Sequence Patterns
;;=============================================================================

(deftest match-fn-sequence-test
  (testing "fixed-length sequence"
    (is (= [1 2] ((match-fn [?a ?b] [?a ?b]) [1 2])))
    (is (failure? ((match-fn [?a ?b] [?a ?b]) [1])))
    (is (failure? ((match-fn [?a ?b] [?a ?b]) [1 2 3]))))

  (testing "rest patterns with ?x*"
    (is (= '(2 3) ((match-fn [?first ?rest*] ?rest) [1 2 3])))
    (is (= '() ((match-fn [?first ?rest*] ?rest) [1]))))

  (testing "one-or-more with ?x+"
    (is (= '(2 3) ((match-fn [?first ?rest+] ?rest) [1 2 3])))
    (is (failure? ((match-fn [?first ?rest+] ?rest) [1])))))

;;=============================================================================
;; Optional Patterns
;;=============================================================================

(deftest match-fn-optional-test
  (testing "optional element ?x?"
    (is (= 2 ((match-fn [?a ?b?] ?b) [1 2])))
    (is (nil? ((match-fn [?a ?b?] ?b) [1])))))

;;=============================================================================
;; Constrained Matching
;;=============================================================================

(deftest match-fn-constrained-test
  ;; NOTE: :when requires runtime eval which needs SCI configured
  ;; Skip :when tests in CLJS for now, covered by Clojure RCT tests
  #?(:clj
     (testing ":when constraint"
       (is (= 4 ((match-fn (?x :when even?) ?x) 4)))
       (is (failure? ((match-fn (?x :when even?) ?x) 3)))))

  (testing ":default on nil"
    (is (= 42 ((match-fn {:a (?x :default 42)} ?x) {:a nil})))
    (is (= 42 ((match-fn {:a (?x :default 42)} ?x) {})))))

;;=============================================================================
;; Variable Unification
;;=============================================================================

(deftest match-fn-unification-test
  (testing "same variable must unify"
    (is (= 1 ((match-fn [?x ?x] ?x) [1 1])))
    (is (failure? ((match-fn [?x ?x] ?x) [1 2])))))

;;=============================================================================
;; Rule-based Transformation
;;=============================================================================

(deftest rule-test
  (testing "rule matches and transforms"
    (let [double-to-add (rule (* 2 ?x) (+ ?x ?x))]
      (is (= '(+ 5 5) (double-to-add '(* 2 5))))
      (is (nil? (double-to-add '(* 3 5))))))

  (testing "rule with multiple variables"
    (let [swap-args (rule (f ?a ?b) (f ?b ?a))]
      (is (= '(f 2 1) (swap-args '(f 1 2)))))))

(deftest apply-rules-test
  (testing "recursive rule application"
    (let [simplify-mul-2 (rule (* 2 ?x) (+ ?x ?x))
          simplify-add-0 (rule (+ 0 ?x) ?x)]
      (is (= '(+ y y)
             (apply-rules [simplify-mul-2 simplify-add-0]
                          '(+ 0 (* 2 y))))))))

;;=============================================================================
;; Failure Handling
;;=============================================================================

(deftest failure-test
  (testing "failure? predicate"
    (is (failure? ((match-fn {:a ?x} ?x) "not a map")))
    (is (not (failure? ((match-fn {:a ?x} ?x) {:a 1})))))

  (testing "failure contains diagnostic info"
    (let [result ((match-fn {:a ?x} ?x) "not a map")]
      (is (failure? result))
      (is (string? (:reason result))))))
