(ns assignment-1.task_1 "Task - Squaring lists")


(defn square-list-recur [v]
  (loop [values v sqValues []]
    (if (empty? values)
      sqValues
      (recur (rest values) (conj sqValues (#(* % %) (first values)))))))

(defn square-list-map [v]
  (map #(* % %) v))



(time (square-list-recur [4 4 2 1 1 43 123 123 223 1 55 23 123 545 12 765]))
(time (square-list-map [4 4 2 1 1 43 123 123 223 1 55 23 123 545 12 765]))
