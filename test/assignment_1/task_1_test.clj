(ns assignment-1.task_1-test
  (:require [clojure.test :refer :all]
            [assignment-1.task_1 :refer :all]))

(deftest square-list-map-test
  (testing "Squaring a list of numbers"
    (testing "by passing in a"
      (testing "vector"
        (testing "of positive data structure number types"
          (is (= '(1 4 9 16.81 25 36 49 64 81 100) (square-list-map [1 2 3 4.1 5 6/1 7 8 9 10]))))
        (testing "of negative data structure number types"
          (is (= '(1 4 11.559999999999999 16 25 36 49 64/9 81 100) (square-list-map [-1 -2 -3.4 -4 -5 -6 -7 -8/3 -9 -10]))))
        (testing "of different data structure values"
          (is (= '(16 4 4.840000000000001) (square-list-map ["foo" true nil '(1 3 4) 4 [2 3 "bar"] 12/6 {:foo 1 :bar 2} -2.2 #{3 2.1 "nice"} :foo])))))
      (testing "list of different data structure values"
        (is (= '(16 4 4.840000000000001) (square-list-map '("foo" true nil '(1 3 4) 4 [2 3 "bar"] 12/6 {:foo 1 :bar 2} -2.2 #{3 2.1 "nice"} :foo)))))
      (testing "map of different data structure values"
        (is (= () (square-list-map {:foo 1 :bar 2}))))
      (testing "hash-set of different data structure values"
        (is (= '(4.840000000000001 16 4) (square-list-map #{"foo" true nil '(1 3 4) 4 [2 3 "bar"] 12/6 {:foo 1 :bar 2} -2.2 #{3 2.1 "nice"} :foo}))))))
  (testing "returns a lazy sequence"
   (is (= false (realized? (square-list-map [1 2 3]))))))

(deftest square-list-recur-test
  (testing "Squaring a list of numbers"
    (testing "by passing in a"
      (testing "vector"
        (testing "of positive data structure number types"
          (is (= '(1 4 9 16.81 25 36 49 64 81 100) (square-list-recur [1 2 3 4.1 5 6/1 7 8 9 10]))))
        (testing "of negative data structure number types"
          (is (= '(1 4 11.559999999999999 16 25 36 49 64/9 81 100) (square-list-recur [-1 -2 -3.4 -4 -5 -6 -7 -8/3 -9 -10]))))
        (testing "of different data structure values"
          (is (= '(16 4 4.840000000000001) (square-list-recur ["foo" true nil '(1 3 4) 4 [2 3 "bar"] 12/6 {:foo 1 :bar 2} -2.2 #{3 2.1 "nice"} :foo])))))
      (testing "list of different data structure values"
        (is (= '(16 4 4.840000000000001) (square-list-recur '("foo" true nil '(1 3 4) 4 [2 3 "bar"] 12/6 {:foo 1 :bar 2} -2.2 #{3 2.1 "nice"} :foo)))))
      (testing "map of different data structure values"
        (is (= () (square-list-recur {:foo 1 :bar 2}))))
      (testing "hash-set of different data structure values"
        (is (= '(4.840000000000001 16 4) (square-list-recur #{"foo" true nil '(1 3 4) 4 [2 3 "bar"] 12/6 {:foo 1 :bar 2} -2.2 #{3 2.1 "nice"} :foo}))))))
  (testing "Returns a lazy sequence"
     (is (= false (realized? (square-list-recur [1 2 3]))))))

(deftest square-test
  (testing "Square function"
    (testing "with positive data structure number types"
      (is (= 176400 (square 420)))
      (is (= 1024 (square 64/2)))
      (is (= 156.25 (square 12.5))))
    (testing "with negative data structure number types"
      (is (= 176400 (square -420)))
      (is (= 1024 (square -64/2)))
      (is (= 156.25 (square -12.5))))))
