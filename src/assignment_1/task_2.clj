(ns assignment-1.task_2 "Task 2 - Change Counter")

(defn change-counter [amount]
  (let [coins [1 5 10 25]]
    (loop [c coins combinations 0]
      (if (empty? c)
       combinations
       (recur)))))
