(ns assignment-1.task_2 "Task 2 - Change Counter")

(defn coin-combination-counter
  [coin amount combinations]
  (loop [x 1 c [1]]
   (cond
     (> x amount) c
     (< x coin) (recur (inc x) (conj c (get combinations x)))
     :else (recur (inc x) (conj c (+ (get combinations x) (get c (- x coin))))))))

(defn change-counter
  ([coins amount combinations]
   (if (empty? coins)
     (last combinations)
     (recur (rest coins) amount (coin-combination-counter (first coins) amount combinations))))
 ([coins amount]
  (change-counter (rest coins) amount (vec (replicate (inc amount) (get coins 0))))))


;; To helo me understand the calculation needed to solve the coin combination problem, I watched the first part of
;; this YouTube video to understand it as it explains the steps involved to calculate it https://www.youtube.com/watch?v=jaNZ83Q3QGc

;;
