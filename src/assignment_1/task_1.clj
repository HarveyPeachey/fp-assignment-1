(ns assignment-1.task_1 "Task 1 - Squaring lists")

(def square #(* % %))

(defn square-list-map [v]
  (map square (filter number? v)))

(defn square-list-recur [v]
  (lazy-seq
    (loop [values (seq v) sqValues []]
      (cond
        (empty? values) sqValues
        (number? (first values)) (recur (rest values) (conj sqValues (square (first values))))
        :else (recur (rest values) sqValues)))))

;; I chose to break down the problem into chunks
;; so I seperated out functionality into functions that made sense to me.

;; I chose to provide two solutions for this problem
;; I first started with the map method which iterates over any given seq
;; this works with all data structures (vector, list, set) apart from maps as the values
;; are stored as a clojure.lang.MapEntry so the function will just spit out an empty lazy-seq.
;; The reason for this is that I used the built in filter method to filter out any data types
;; that weren't a number using number?.

;; I then created another solution that uses recursion
;; The reason I chose this as another solution is because I wanted to remove the extra iteration
;; over the sequence that filter does to remove/clean unwanted data types.
;; I used the loop construct and recur to iterate over the sequence to apply both
;; the check for the value being a number and skipping over a value and not conjing back onto the accumalted vector
;; this meant it would do both with only one pass through of the given sequence.
;; I also chose a vector to accumlate as I wanted the ability to add to the end of the sequence which list doesn't allow.

;; There probably was a way to implement it using the built in transducers but I thought it would be easier
;; to use recursion to solve it.
