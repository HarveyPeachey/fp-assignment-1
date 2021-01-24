(ns assignment-1.task_2 "Task 2 - Change Counter"
  (:require [clojure.spec.alpha :as s]))

;; To help me understand the calculation needed to solve the coin combination problem, I watched the first part of
;; this YouTube video to understand it as it explains the steps involved to calculate it https://www.youtube.com/watch?v=jaNZ83Q3QGc
;; I've also split the solution into 2 separate functions. With use of spec to validate inputted function data.

;; Function change-counter -------------------------------------------------------------------------------------------------------------------------
;; The main entry point to start the combinations counting process, the let block function re-binds coins so it's in a sorted format
;; as the order of the coins should be numerical, it also binds combinations to a vector of the lowest of the coin amounts which is
;; which skips the first step of the algorithm shown in the video.
;; Next there is a check for the edge case of the first coin being bigger than the amount, or there not being a coin of with a value of 1
;; Lastly it enters a loop-recur block which iterates through each coin in the given sequence and calls the other function to calculate
;; the amount of combinations for that particular coin in the iteration. The first coin is skipped as that has been processed already,
;; as explained in the previous paragraph by rebinding coins to the (rest coins).
;; I have also added a 0-arity parameter overload to use the default coins provided by the question.

;; Function coin-combination -----------------------------------------------------------------------------------------------------------------------
;; This function takes a single coin integer value, the amount that coin has to make combinations for and the combinations previously
;; calculated. It then follows the steps shown in the video for solving how many combinations but using recursion.
;; The loop is setup with an incrementor of x and a vector called c which starts with a integer of 1, due to position 0 always having a value of 1
;; this is followed by a cond statement which checks to see if the x binding is more than the amount, which will return the combinations calculated.
;; The next part is used to skip calculations if x is below the coin value, it then conj's the the value from the original combinations
;; onto the new combinations vector c. The else branch calculates the permutations by adding together both the value in the original
;; combinations to the new combinations by getting the integer position of x minus the value of coin in the new combinations vector. It then uses
;; recursion to increment x and move onto the next amount.

;; Resources that helped me a lot for this assignment
;; https://www.braveclojure.com/do-things/
;; https://www.braveclojure.com/core-functions-in-depth/
;; https://jafingerhut.github.io/cheatsheet/clojuredocs/cheatsheet-tiptip-cdocs-summary.html
;; https://clojuredocs.org/

(s/def ::coin integer?)

(s/def ::amount integer?)

(s/def ::coins
  (s/coll-of ::coin :distinct true))

(s/def ::combinations
  (s/coll-of integer? :kind vector?))

(defn coin-combination-counter
  "Calculates the amount of combinations with the given coin, amount and previous combination calculations"
  [coin amount combinations]
  {:pre [(s/valid? ::coin coin)
         (s/valid? ::amount amount)
         (s/valid? ::combinations combinations)
         (= (count combinations) (inc amount))]}
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
   (let [coins (sort coins) combinations (vec (replicate (inc amount) 1))]
     (if (or (not= (first coins) 1) (< amount (first coins)))
      "Sorry bro your coins are too big and/or have a coin with a value of 1"
      (loop [coins (rest coins) c combinations]
        (if (empty? coins)
          (str "Combinations for an amount of " amount " is " (last c))
          (recur (rest coins) (coin-combination-counter (first coins) amount c)))))))
  ([amount]
   (change-counter [1 5 10 25] amount)))
