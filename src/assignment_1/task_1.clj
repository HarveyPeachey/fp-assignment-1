(ns assignment-1.task_1 "Task - Squaring lists")


(defn squareList [v]
  (loop [values v sqValues []]
    (if (empty? values)
      sqValues
      (recur (rest values) (conj sqValues (#(* % %) (first values)))))))


(squareList [4 4 2 1 1])
