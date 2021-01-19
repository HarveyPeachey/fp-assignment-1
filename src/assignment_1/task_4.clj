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

(defn lookup-month-name [month]
  "Used to lookup the name of a month"
  (let [month-names ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]]
    (get month-names month)))

(defn warmest-day-each-month
  []
  (map
    (fn [[key values]]
      (str (lookup-month-name (- key 1)) " " (str/join " " ((juxt :day :year :temperature) (last (sort-by :temperature values))))))
    (sort-by first (group-by :month (get-formatted-data-memo)))))

(defn average
  [numbers]
  (/ (reduce + numbers) (count numbers)))

(defn mean-temp-each-year
  []
  (map
    (fn [[key values]]
      {:year key
       :temperature (average (map :temperature values))})
    (group-by :year (get-formatted-data-memo))))

(defn find-warmest-and-coldest-year
  "Finds warmest year"
  []
  (do (str "Warmest: " (str/join " " ((juxt :year :temperature) (last (sort-by :temperature (mean-temp-each-year))))) " "
           "Coldest: " (str/join " " ((juxt :year :temperature) (first (sort-by :temperature (mean-temp-each-year))))))))

(defn mean-temp-each-month
  []
  (map
    (fn [[key values]]
      {:month key
       :temperature (average (map :temperature values))})
    (group-by :month (get-formatted-data-memo))))

(defn mean-temp-each-month-year
  []
  (map
    (fn [[key values]]
      {:year (first key)
       :month (second key)
       :temperature (average (map :temperature values))})
    (group-by (juxt :year :month) (get-formatted-data-memo))))

(defn smallest-variation [x coll]
  (take 1 (sort-by :temperature #(< (Math/abs (- x %1)) (Math/abs (- x %2))) coll)))

(defn greatest-variation [x coll]
  (take 1 (sort-by :temperature #(> (Math/abs (- x %1)) (Math/abs (- x %2))) coll)))

(defn greatest-and-smallest-variation
  []
  (map
    (fn [[key values] value2]
       (str "Greatest and Smallest Variation for " (lookup-month-name (- key 1)) " "
            (pr-str (concat (map :year (greatest-variation (:temperature value2) values))
                            (map :year (smallest-variation (:temperature value2) values))))))
    (sort-by first (group-by :month (mean-temp-each-month-year))) (sort-by :month (mean-temp-each-month))))
