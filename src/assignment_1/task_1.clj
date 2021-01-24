(ns assignment-1.task_1 "Task 1 - Squaring Lists")

;; I chose to break down the problem into chunks so I separated out functionality into functions that made sense to me.
;; I also opted to not use spec in this task as I implemented spec in Tasks 2 and 4 where 4 contains a lot of spec use.
;; I used normal methods to catch edge cases.

;; I chose to provide two solutions for this problem
;; I first started with the map method which iterates over any given seq
;; this works with all data structures (vector, list, set) apart from maps as the values
;; are stored as a clojure.lang.MapEntry so the function will just spit out an empty lazy-seq.
;; The reason for this is that I used the built in filter method to filter out any data types
;; that weren't a number using number?.

;; I then created another solution that uses recursion. The reason I chose this as another solution is because I wanted to remove the extra iteration
;; over the sequence that filter does to remove/clean unwanted data types. I used the loop construct and recur to iterate over the sequence to apply both
;; the check for the value being a number and skipping over a value and not conjing back onto the accumulated vector this meant it would do both with only
;; one pass through of the given sequence. I also chose a vector to accumulate as I wanted the ability to add to the end of the sequence which list doesn't allow.
;; Unfortunately this makes the loop eager as it will have to realise the sqValues each time conj is used to add the next square value so this may
;; make it slower anyway.

;; There probably was a way to implement it using the built in transducers but I thought it would be easier
;; to use recursion to solve it.

;; Resources that helped me a lot for this assignment
;; https://www.braveclojure.com/do-things/
;; https://www.braveclojure.com/core-functions-in-depth/
;; https://jafingerhut.github.io/cheatsheet/clojuredocs/cheatsheet-tiptip-cdocs-summary.html
;; https://clojuredocs.org/

(defn square
  "Returns the squared value of a given number"
  [x]
  {:pre [(number? x)]}
  (* x x))

(defn square-list-map
  [values]
  {:pre [(coll? values)]}
  (map square (filter number? values)))

(defn square-list-recur
  [values]
  {:pre [(coll? values)]}
  (lazy-seq
    (loop [v values sqValues []]
        (cond
          (empty? v) sqValues
          (number? (first v)) (recur (rest v) (conj sqValues (square (first v))))
          :else (recur (rest v) sqValues)))))
