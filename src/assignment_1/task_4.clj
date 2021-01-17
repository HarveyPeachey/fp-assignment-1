(ns assignment-1.task_4
  (:require [clojure.string :as str]))

(defn get-data
  "Slurps the met office weather data and stores each line in a two dimensional array"
  []
  (mapv vec (partition 14 (mapv #(Float/parseFloat %) (str/split (str/triml (slurp "https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat")) #"\s+")))))

(defn create-record
  [data]
  (loop [x 2 a []]
    (if (> x 13)
      a
      (recur (inc x) (conj a (hash-map :year (get data 0) :month (- x 1) :day (get data 1) :temperature (get data x)))))))

(defn get-formatted-data
  []
  (mapcat #(create-record %) (get-data)))

(def get-formatted-data-memo (memoize get-formatted-data))

(defn find-warmest-day-each-month
  "Finds warmest day for each calendar month"
  []
  (loop [m 1 r []]
    (if (> m 12)
      r
      (recur (inc m) (conj r (apply max (map :temperature (filter #(= (:month %) m) (vec (get-formatted-data-memo))))))))))

(defn find-warmest-year
  "Finds warmest year"
  []
  (reduce + (map :temperature (filter #(= (:year %) 1772) data))))

(defn find-mean-temp-month
  []
  (reduce + (map :temperature (filter #(= (:year %) 1772) data))))
