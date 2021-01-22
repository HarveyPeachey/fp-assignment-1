(ns assignment-1.task_2-test
  (:require [clojure.test :refer :all]
            [assignment-1.task_2 :refer :all]))

(deftest coin-combination-counter-test
  (testing "Coin combination counter with"
    (testing "valid input parameters"
      (is (= [1 1 1 1 1 2 2 2 2 2 3]
             (coin-combination-counter 5 10 [1 1 1 1 1 1 1 1 1 1 1]))))
    (testing "invalid input parameter assertions"
      (are [x] (thrown? AssertionError x)
        (coin-combination-counter 5 100 [1 1 1 1 1 1 1 1 1 1 1])
        (coin-combination-counter 5.0 10 [1 1 1 1 1 1 1 1 1 1 1])
        (coin-combination-counter 5 10.0 [1 1 1 1 1 1 1 1 1 1 1])))))

(deftest change-counter-test
  (testing "Coin change counter with"
    (testing "valid input parameters"
      (is (= "Combinations for an amount of 100 is 242"
             (change-counter 100)) "for $1 with default coins 1 5 10 25")
      (is (= "Combinations for an amount of 100000 is 13398445413854501"
             ;; Note this is the value produced by my machines repl however
             ;; using an online repl with my code produces 13398445413854540
             ;; probably due to the way integers are being handled for some reason
             ;; so this test may fail on another machine.
             (change-counter [1 5 10 25 50 100] 100000)) "for $100 with default coins 1 5 10 25 50 100"))
    (testing "invalid input parameter"
      (testing "assertions"
        (are [x] (thrown? AssertionError x)
          (change-counter [5.1] 100)
          (change-counter [1 5 10 25] 100.0)))
      (testing "values"
       (is (= "Sorry bro your coins are too big and/or have a coin with a value of 1"
              (change-counter [2 3 4 5] 100)) "of a coin set with no 1 coin value")
       (is (= "Sorry bro your coins are too big and/or have a coin with a value of 1"
              (change-counter [100] 10)) "with coin set that is larger than amount given")))))
