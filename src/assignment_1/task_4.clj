(ns assignment-1.task_4 "Task 4 - Temperature records"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

;; Spec definitions---------------------------------------------------------------------------------------------------------------------------------
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

(s/def ::weather-record-month
  (s/keys :req-un [::month ::temperature]))

(s/def ::weather-record-year
  (s/keys :req-un [::year ::temperature]))

(s/def ::weather-record-month-year
  (s/keys :req-un [::month ::year ::temperature]))

(s/def ::weather-data
  (s/coll-of ::weather-record))

(s/def ::weather-data-year
  (s/coll-of ::weather-record-year))

(s/def ::weather-data-month
  (s/coll-of ::weather-record-month))

(s/def ::weather-data-month-year
  (s/coll-of ::weather-record-month-year))

;; Data retrieval, parsing and formatting ----------------------------------------------------------------------------------------------------------
(defn get-data
  "Slurps from the given url with split rules and stores each line in a two dimensional array"
  ([url line-split regex-split]
   (try
       (as-> (slurp url) x
             (str/triml x)
             (str/split x regex-split)
             (mapv #(Integer/parseInt %) x)
             (partition line-split x)
             (mapv vec x))
       (catch Exception e (println (str "caught exception: " e)))))
  ([]
   (get-data "https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat" 14 #"\s+")))

(defn make-weather-record
  "Creates a record-like hash-map by mapping values to corresponding keys"
  [year month day temperature]
  {:post [(s/valid? ::weather-record %)]}
  (hash-map :year year
            :month month
            :day day
            :temperature (float temperature)))

(defn create-weather-records
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
  (mapcat #(create-weather-records %) (get-data)))

(def get-formatted-data-memo
  "Memoizes the get-formatted-data function so it doesn't have to process the data when recalled"
  (memoize get-formatted-data))

;; Task 4 helper functions------------------------------------------------------------------------------------------------------------------------
(defn lookup-month-name
  "Used to lookup the name of a month"
  [month]
  (let [month-names ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]]
    (get month-names month)))

(defn average
  "Used to calculate the mean for a collection of numbers"
  [numbers]
  (/ (reduce + numbers) (count numbers)))

(defn smallest-variation
  "Sorts a given collection and gets the smallest variation from that temperature"
  [x coll]
  (take 1 (sort-by :temperature #(< (Math/abs (- x %1)) (Math/abs (- x %2))) coll)))

(defn greatest-variation
  "Sorts a given collection and gets the greatest variation from that temperature"
  [x coll]
  (take 1 (sort-by :temperature #(> (Math/abs (- x %1)) (Math/abs (- x %2))) coll)))

;; Task 4 question solutions------------------------------------------------------------------------------------------------------------------------
(defn warmest-day-each-month
  "Displays the warmest day for each calendar month"
  ([data]
   {:pre [(s/valid? ::weather-data data)]}
   (map
     (fn [[key values]]
       (str (lookup-month-name (- key 1)) " " (str/join " " ((juxt :day :year :temperature) (last (sort-by :temperature values))))))
     (sort-by first (group-by :month data))))
  ([]
   (warmest-day-each-month (get-formatted-data-memo))))

(defn mean-temp-each-year
  "Calculates the mean temperature for each year"
  ([data]
   {:pre [(s/valid? ::weather-data data)]}
   (map
     (fn [[key values]]
       {:year key
        :temperature (average (map :temperature values))})
     (group-by :year data)))
  ([]
   (mean-temp-each-year (get-formatted-data-memo))))

(defn warmest-and-coldest-year
  "Displays the warmest and coldest years"
  ([data]
   {:pre [(s/valid? ::weather-data-year data)]}
   (do (str "Warmest year was " (str/join " at a temperature of " ((juxt :year :temperature) (last (sort-by :temperature data)))) " "
            "and Coldest year was " (str/join " at a temperature of " ((juxt :year :temperature) (first (sort-by :temperature data)))))))
  ([]
   (warmest-and-coldest-year (mean-temp-each-year))))

(defn mean-temp-each-month
  "Calculates the mean temperature for each month"
  ([data]
   {:pre [(s/valid? ::weather-data data)]}
   (map
     (fn [[key values]]
       {:month key
        :temperature (average (map :temperature values))})
     (group-by :month data)))
  ([]
   (mean-temp-each-month (get-formatted-data-memo))))

(defn mean-temp-each-month-year
  "Calculates the mean temperature for each month in every year"
  ([data]
   {:pre [(s/valid? ::weather-data data)]}
   (map
     (fn [[key values]]
       {:year (first key)
        :month (second key)
        :temperature (average (map :temperature values))})
     (group-by (juxt :year :month) data)))
  ([]
   (mean-temp-each-month-year (get-formatted-data-memo))))

(defn greatest-and-smallest-variation
  "Displays the greatest and smallest variation from the mean temperature of each month"
  ([data1 data2]
   {:pre [(s/valid? ::weather-data-month-year data1)
          (s/valid? ::weather-data-month data2)]}
   (map
     (fn [[key coll1] coll2]
        (str "Variation against " (lookup-month-name (- key 1)) " for Greatest: "
             (str/join " and Smallest: " (concat (map :year (greatest-variation (:temperature coll2) coll1))
                                                 (map :year (smallest-variation (:temperature coll2) coll1))))))
     (sort-by first (group-by :month data1)) (sort-by :month data2)))
  ([]
   (greatest-and-smallest-variation (mean-temp-each-month-year) (mean-temp-each-month))))

(defn warmest-leap-day
  "Displays the warmest leap day which is the 29th of February on every leap year"
  ([data]
   {:pre [(s/valid? ::weather-data data)]}
   (str "Warmest leap day was in " (str/join " at a temperature of " ((juxt :year :temperature) (first (sort-by :temperature #(> %1 %2) (filter #(and (= (:month %) 2) (= (:day %) 29)) data)))))))
  ([]
   (warmest-leap-day (get-formatted-data-memo))))

(defn mean-temp-each-century
  "Calculates the mean temperature for each century"
  ([data]
   {:pre [(s/valid? ::weather-data data)]}
   (map
     (fn [[key values]]
       {:century (+ key 1)
        :temperature (average (map :temperature values))})
     (group-by #(quot (:year %) 100) data)))
  ([]
   (mean-temp-each-century (get-formatted-data-memo))))

;; FOOTNOTE: Please read the bottom of task_4_old.clj for an explanation on my original implementation of this task
