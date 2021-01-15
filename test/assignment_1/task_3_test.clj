(ns assignment-1.task_3-test
  (:require [clojure.test :refer :all]
            [assignment-1.task_3 :refer :all]))

(deftest find-plants-test
  (testing "Finding plants"
    (testing "when called with"
      (testing "a childs name as a string"
        (is (= ["Clover" "Grass" "Grass" "Clover"] (find-plants "Ginny")))
        (is (= ["Violet" "Violet"] (find-plants "Alice" "VVVVV")) "and a correct garden format")
        (is (= "Uh oh, that's a not a garden..." (find-plants "Alice" '("VVV"))) "and an incorrect garden format")))))

(deftest format-garden-test
  (testing "Garden formatter"
    (testing "when called with"
      (testing "a string length of 24"
        (is (= [["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W"]]
               (format-garden "ABCDEFGHIJKLMNOPQRSTUVW"))))
      (testing "a string of more than 24"
        (is (= [["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W"]
                ["Y" "Z"]]
               (format-garden "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))))))
