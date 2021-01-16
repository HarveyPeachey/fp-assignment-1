(ns assignment-1.task_3-test
  (:require [clojure.test :refer :all]
            [assignment-1.task_3 :refer :all]))

(deftest find-plants-test
  (testing "Finding plants"
    (testing "when called with"
      (testing "a childs name as a string"
        (is (= ["Clover" "Grass" "Grass" "Clover"]
               (find-plants "Ginny")))
        (is (= ["Violet" "Violet"]
               (find-plants "Alice" "VVVVV")) "and a correct garden format")
        (is (= "Uh oh, that's a not a garden..."
               (find-plants "Alice" '("VVV"))) "and an incorrect garden format")))))

(deftest format-garden-test
  (testing "Garden formatter"
    (testing "when called with"
      (testing "a string length of 24"
        (is (= [[\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X]]
               (format-garden "ABCDEFGHIJKLMNOPQRSTUVWX"))))
      (testing "a string length of more than 24"
        (is (= [[\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X]
                [\Y \Z]]
               (format-garden "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))))))

(deftest find-childs-plants-test
  (let [garden  [[\V \R \C \G \V \V \R \V \C \G \G \C \C \G \V \R \G \C \V \C \G \C \G \V]
                 [\V \R \C \C \C \G \C \R \R \G \V \C \G \C \R \V \V \C \V \G \C \G \C \V]]
        garden2 [[\V \R \C \G \V \V \R \V \C \G \G \C \C \G \V \R \G \C \V \C \G \C \G \V]
                 [\V \R \C \C \C \G \C \R \R \G \V \C \G \C \R \V \V \C \V \G \C \G \C \V]
                 [\G]]]
    (testing "Finding a childs plants"
      (testing "when called with"
        (testing "a given child string"
          (testing "that has a default garden of 48 plants"
            (is (= ["Violet" "Radish" "Violet" "Radish"]
                   (find-childs-plants "Alice" garden)) "exists in the class")
            (is (= "Feed me, Seymour! That child doesn't like plants"
                   (find-childs-plants "Harvey" garden)) "doesn't exist in the class"))
          (testing "that has a obscure garden that's added an extra row"
            (is (= ["Violet" "Radish" "Violet" "Radish" "Grass"]
                   (find-childs-plants "Alice" garden2)) "with a child who has a plant on it")
            (is (= ["Clover" "Grass" "Clover" "Clover"]
                   (find-childs-plants "Bob" garden2)) "with a child who hasn't got a plant on it")))))))


(deftest lookup-plant-name-test
  (testing "Looking up a plant name"
    (testing "when given a shorthand"
      (is (= ["Violet"] (lookup-plant-name "V")) "that exists")
      (is (= [nil] (lookup-plant-name "P")) "that doesn't exist"))))
