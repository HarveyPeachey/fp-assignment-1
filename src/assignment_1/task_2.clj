(ns assignment-1.task_2 "Task 2 - Change Counter")


(defn change-counter
  ([coins amount combinations]
   (if (empty? coins)
     (last combinations)
     (recur (rest coins) amount (loop [x 1 c [1]]
                                  (cond
                                    (> x amount) c
                                    (< x (first coins)) (recur (inc x) (conj c (get combinations x)))
                                    :else (recur (inc x) (conj c (+ (get combinations x) (get c (- x (first coins)))))))))))


  ([coins amount]
   (change-counter (rest coins) amount (vec (replicate (inc amount) (get coins 0))))))
