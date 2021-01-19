(ns assignment-1.task_4
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(s/def ::year
  (s/and #(<= 1772 %)
         #(>= 2020 %)
         integer?))
(s/def ::month
  (s/and #(<= 1 %)
         #(>= 12 %)
         integer?))
(s/def ::day
  (s/and #(<= 1 %)
         #(>= 31 %)
         integer?))
(s/def ::temperature float?)
(s/def ::weather-record
  (s/keys :req-un [::day ::month ::year ::temperature]))

(def data [{:year 1 :month 1}
           {:year 2 :month 1}
           {}])


(defn get-data
  "Slurps the met office weather data and stores each line in a two dimensional array"
  []
  (as-> (slurp "https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat") x
        (str/triml x)
        (str/split x #"\s+")
        (mapv #(Integer/parseInt %) x)
        (partition 14 x)
        (mapv vec x)))

(defn make-weather-record
  [year month day temperature]
  {:post [(s/valid? ::weather-record %)]}
  (hash-map :year year
            :month month
            :day day
            :temperature (float temperature)))

(defn create-record
  "Breaks down the row of data into a sequence of hash-maps"
  [row-data]
  (loop [x 2 a []]
    (cond
      (> x 13) a
      (= (get row-data x) -999) (recur (inc x) a)
      :else (recur (inc x) (conj a (make-weather-record (get row-data 0)
                                                        (- x 1)
                                                        (get row-data 1)
                                                        (get row-data x)))))))

(defn get-formatted-data
  "Formats the fetched data into a vector of hash-maps"
  []
  (mapcat #(create-record %) (get-data)))

(def get-formatted-data-memo
  "Memoizes the get-formatted-data function so it doesn't have to process the data when recalled"
  (memoize get-formatted-data))

(defn get-data-by-month
  []
  (sort-by :month (get-formatted-data-memo)))

(defn get-data-by-year
  []
  (sort-by :year (get-formatted-data-memo)))

(defn get-data-by-year-month
  []
  (sort-by (juxt :year :month) (get-formatted-data-memo)))

(def get-data-by-month-memo
  (memoize get-data-by-month))

(def get-data-by-year-memo
  (memoize get-data-by-year))

(def get-data-by-year-month-memo
  (memoize get-data-by-year-month))

(defn lookup-month-name [month]
  "Used to lookup the name of a month"
  (let [month-names ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]]
    (get month-names month)))

(defn find-warmest-day-each-month
  "Finds warmest day for each calendar month"
  []
  (loop [m 1 r []]
    (if (> m 12)
      r
      (recur (inc m) (conj r (->> (get-data-by-month-memo)
                                  (drop-while #(< (:month %) m))
                                  (take-while #(< (:month %) (+ m 1)))
                                  (sort-by :temperature)
                                  (last)))))))

(defn average
  [numbers]
  (/ (reduce + numbers) (count numbers)))

(defn average-year-temps
  []
  (loop [y 1772 r []]
    (if (> y 2020)
      r
      (recur (inc y) (conj r {:year y :temperature (->> (get-data-by-year-memo)
                                                        (drop-while #(< (:year %) y))
                                                        (take-while #(< (:year %) (+ y 1)))
                                                        (map :temperature)
                                                        (average))})))))

(def average-year-temps-memo
  "Memoizes the get-formatted-data function so it doesn't have to process the data when recalled"
  (memoize average-year-temps))

(defn find-warmest-and-coldest-year
  "Finds warmest year"
  []
  (do (str "Warmest: " ((juxt :year :temperature) (last (sort-by :temperature (average-year-temps-memo)))) " "
           "Coldest: " ((juxt :year :temperature) (first (sort-by :temperature (average-year-temps-memo)))))))

(defn mean-temp-month
  ([]
   (loop [m 1 r []]
     (if (> m 12)
       r
       (recur (inc m) (conj r {:month m :temperature (->> (get-data-by-month-memo)
                                                          (drop-while #(< (:month %) m))
                                                          (take-while #(< (:month %) (+ m 1)))
                                                          (map :temperature)
                                                          (average))})))))
  ([y]
   (loop [m 1 r []]
     (if (> m 12)
       r
       (recur (inc m) (conj r {:year y :month m :temperature (->> (get-data-by-year-month-memo)
                                                                  (drop-while #(< (:year %) y))
                                                                  (take-while #(< (:year %) (+ y 1)))
                                                                  (drop-while #(< (:month %) m))
                                                                  (take-while #(< (:month %) (+ m 1)))
                                                                  (map :temperature)
                                                                  (average))}))))))

(defn mean-temp-month-each-year
  []
  (loop [y 1772 r []]
    (if (> y 2020)
      r
      (recur (inc y) (concat r (mean-temp-month y))))))

(def mean-temp-month-each-year-memo
  (memoize mean-temp-month-each-year))

(defn smallest-variation [x items]
  (take 1 (sort-by :temperature #(< (Math/abs (- x %1)) (Math/abs (- x %2))) items)))

(defn greatest-variation [x items]
  (take 1 (sort-by :temperature #(> (Math/abs (- x %1)) (Math/abs (- x %2))) items)))

(greatest-variation 34.538411711361576 (filter #(= (:month %) 1) (mean-temp-month-each-year-memo)))
