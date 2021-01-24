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

(s/def ::row-data
  (s/coll-of integer? :count 14 :kind vector?))

(s/def ::raw-data
  (s/coll-of ::row-data))

;; Data retrieval, parsing and formatting ----------------------------------------------------------------------------------------------------------
(defn get-data
  "Slurps from the given url with split rules and stores each line as a vector contained in a sequence"
  ([url line-split regex-split]
   (try
       (as-> (slurp url) x
             (str/triml x)
             (str/split x regex-split)
             (map #(Integer/parseInt %) x)
             (partition line-split x)
             (map vec x))
       (catch Exception e (str "caught exception: " e))))
  ([]
   (get-data "https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat" 14 #"\s+")))

(defn make-weather-record
  "Creates a record-like hash-map by mapping values to corresponding keys"
  [year month day temperature]
  {:pre [(s/valid? ::year year)
         (s/valid? ::month month)
         (s/valid? ::day day)
         (s/valid? number? temperature)]}
  {:post [(s/valid? ::temperature temperature)]}
  (hash-map :year year
            :month month
            :day day
            :temperature (float temperature)))

(defn create-weather-records
  "Breaks down the row of data into a sequence of hash-maps"
  [row-data]
  {:pre [(s/valid? ::row-data row-data)]}
  (loop [x 2 a []]
    (cond
      (> x 13) a
      (= (get row-data x) -999) (recur (inc x) a)
      :else (recur (inc x) (conj a (make-weather-record (get row-data 0)
                                                        (- x 1)
                                                        (get row-data 1)
                                                        (get row-data x)))))))

(defn get-formatted-data
  "Formats the fetched data by mapping over each row creating weather records"
  ([raw-data]
   {:pre [(s/valid? ::raw-data raw-data)]}
   (mapcat #(create-weather-records %) raw-data))
  ([]
   (get-formatted-data (get-data))))

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

;; !FOOTNOTE!: Please read the bottom of task_4_old.clj for an explanation on my original implementation of this task

;; Spec definitions --------------------------------------------------------------------------------------------------------------------------------------------
;; At the top of my file I've decided to list all my spec definiions for the data I use with all of my functions throughout this task to validate arguments.

;; Data retrieval, parsing and formatting ----------------------------------------------------------------------------------------------------------------------
;; When I fist tackled this task I spent most of my time trying to think of the best structure I could use to store my data to manipulate it easily, so I
;; stored them as "records" which is a hash-map with each one storing year, month, day and temperature. I know that records do exist but I followed a flow-chart
;; to choose hash-maps over them https://cemerick.com/blog/2011/07/05/flowchart-for-choosing-the-right-clojure-type-definition-form.html. Another reason
;; for the choice is that I didn't need the polymorphism features of defrecords and also having to deal with the defrecords interface which was uneccesary with
;; the dataset provided. My get-data splits each row of the raw data into a sequence of vectors containing integers. I then format the data into my "record"
;; weather-data format. At this point all the -999 values are removed from the dataset. I also memoize my get-formatted-data so it doesn't have to reformat the
;; data every single time my solution functions call it, thus increasing performance of the calculations they do. I also had to use vectors for each row of my
;; raw data to be able to use get to retrieve the data fast, as using nth traverses the whole collection whereas get only has to do a few hops.
;; I also think that a lot of this process could be mapped concurrently which multiple rows being mapped at the same time as the order of each weather-data
;; record doesn't matter at this point of the program.

;; Task 4 helper functions--------------------------------------------------------------------------------------------------------------------------------------
;; This is the section for my helper functions which are mostly re-used by my solution functions. Sort-by was also a crucial sequence manipulator for my data,
;; this made finding the greates and smallest variation very simple as sort-by allowed me to overwrite the default comparator, so I could use Math/abs to
;; calculate them.

;; Task 4 question solutions------------------------------------------------------------------------------------------------------------------------------------
;; Looking at my task 4 solutions I could have made some improvements, the biggest was probably creating a HOC (High order function) to reduce repeated code.
;; The functions which calculate the mean using group-by, could have been compressed into a HOC that takes in the function that group-by applies to my formatted
;; data, along with the keys they will transpose to within the maps anonymous function, along with the calculation, in this case average, which is applied to
;; each temperature. Using juxt also made organising my data very simple, as it allowed me to sort by multiple keys in my data as keywords behave as functions.
;; For my extra questions I chose to find the warmest leap year and the mean temperature for each century. The mean temperature for each century was a bit
;; tricky as the data don't start at the begining of a century and doesn't end in a whole century. Luckily quotient exists in clojure so I used this to group
;; my data by century and did the average calculations on the temperatures.


;; Resources that helped me a lot for this assignment
;; https://www.braveclojure.com/do-things/
;; https://www.braveclojure.com/core-functions-in-depth/
;; https://jafingerhut.github.io/cheatsheet/clojuredocs/cheatsheet-tiptip-cdocs-summary.html
;; https://clojuredocs.org/
