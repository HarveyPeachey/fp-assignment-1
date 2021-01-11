(ns assignment-1.task_1-test
  (:require [clojure.test :refer :all]
            [assignment-1.task_1 :refer :all]))

(deftest square-list-recur-test
  (testing "Squaring list function"
    (testing "passing in a vector"
      (testing "of positive integers"
        (is (= [1 4 9 16 25 36 49 64 81 100] (square-list-recur [1 2 3 4 5 6 7 8 9 10])))
        (is (= [1764 10000 24025 440896 1000000] (square-list-recur [42 100 155 664 1000]))))
      (testing "of negative integers"
        (is (= [1 4 9 16 25 36 49 64 81 100] (square-list-recur [-1 -2 -3 -4 -5 -6 -7 -8 -9 -10])))
        (is (= [1764 10000 24025 440896 1000000] (square-list-recur [-42 -100 -155 -664 -1000]))))
      (testing "of different non numeric data type values"
        (is (= ["1 4 9 16 25 36 49 64 81 100"] (square-list-recur ["yeah" true nil])))))
    (testing "passing in a string")))
