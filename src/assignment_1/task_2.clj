(ns assignment-1.task_2 "Task 2 - Change Counter"
  (:require [clojure.spec.alpha :as s]))

(s/def ::coin integer?)

(s/def ::amount integer?)

(s/def ::coins
  (s/coll-of ::coin))

(s/def ::combinations
  (s/coll-of integer?))

(defn coin-combination-counter
  "Calculates the amount of combinations with the given coin, amount and previous combination calculations"
  [coin amount combinations]
  {:pre [(s/valid? ::coin coin)
         (s/valid? ::amount amount)
         (s/valid? ::combinations combinations)]}
  (loop [x 1 c [1]]
   (cond
     (> x amount) c
     (< x coin) (recur (inc x) (conj c (get combinations x)))
     :else (recur (inc x) (conj c (+ (get combinations x) (get c (- x coin))))))))

(defn change-counter
  "Used to go through each coin in the given vector and produces the total amount of permutations"
  ([coins amount]
   {:pre [(s/valid? ::coins coins)
          (s/valid? ::amount amount)]}
   (let [combinations (vec (replicate (inc amount) (first coins)))]
     (loop [coins (rest coins) c combinations]
       (if (empty? coins)
         (str "Combinations for an amount of " amount " is " (last c))
         (recur (rest coins) (coin-combination-counter (first coins) amount c))))))
  ([amount]
   (change-counter [1 5 10 25] amount)))

;; To helo me understand the calculation needed to solve the coin combination problem, I watched the first part of
;; this YouTube video to understand it as it explains the steps involved to calculate it https://www.youtube.com/watch?v=jaNZ83Q3QGc

;; Function change-counter -----------------------------------------------------------------------------------------------------------
;; The main entry point to start the combinations counting process,
