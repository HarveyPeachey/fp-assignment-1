(ns assignment-1.task_2 "Task 2 - Change Counter")

(def coins [1 5 10 25])

(defn change-counter [coins amount]
  (cond (= amount 0) 1
        (or (< amount 0) (empty? coins)) 0
        :else
          (+ (recur (rest c) a) (recur c (- a (first c))))))


(change-counter [1 5 10 25] 100)
