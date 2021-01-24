(ns assignment-1.task_4-test
  (:require [clojure.test :refer :all]
            [assignment-1.task_4 :refer :all]))

(deftest get-data-test
  (testing "Retrieving data with the get-data function"
    (is (= "caught exception: java.io.FileNotFoundException: https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.da"
           (get-data "https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.da" 14 #"\s+")) "with an invalid slurp url")
    (is (= [1772 1 32 -15 18 25 87 128 187 177 105 111 78 112]
           (first (get-data)))) "with the valid default slurp url"))

(deftest make-weather-record-test
  (testing "Creating a weather data record"
    (is (= {:year 1772 :month 1 :day 19 :temperature 25.0}
           (make-weather-record 1772 1 19 25)) "that is in the correct format")
    (testing "with invalid parameters"
      (are [x] (thrown? AssertionError x)
        (make-weather-record 1765 1 19 25)
        (make-weather-record 1765 113 19 25)
        (make-weather-record 1765 1 192 25)
        (make-weather-record 1765 1 19 "25")))))

(deftest create-weather-records-test
  (testing "Creating a group of weather data records with row-data"
    (is (= [{:day 1, :month 1, :year 1772, :temperature 32.0}
            {:day 1, :month 2, :year 1772, :temperature -15.0}
            {:day 1, :month 3, :year 1772, :temperature 18.0}
            {:day 1, :month 4, :year 1772, :temperature 25.0}
            {:day 1, :month 5, :year 1772, :temperature 87.0}
            {:day 1, :month 6, :year 1772, :temperature 128.0}
            {:day 1, :month 7, :year 1772, :temperature 187.0}
            {:day 1, :month 8, :year 1772, :temperature 177.0}
            {:day 1, :month 9, :year 1772, :temperature 105.0}
            {:day 1, :month 10, :year 1772, :temperature 111.0}
            {:day 1, :month 11, :year 1772, :temperature 78.0}
            {:day 1, :month 12, :year 1772, :temperature 112.0}]
           (create-weather-records [1772 1 32 -15 18 25 87 128 187 177 105 111 78 112])) "that is in the correct format"))
  (testing "that is invalid"
    (are [x] (thrown? AssertionError x)
      (create-weather-records ["foo" true nil '(1 3 4) 4 [2 3 "bar"] 12/6 {:foo 1 :bar 2} -2.2 #{3 2.1 "nice"} :foo])
      (create-weather-records "foo")
      (create-weather-records '(1772 1 32 -15 18 25 87 128 187 177 105 111 78 112))
      (create-weather-records [1772 1 32 -15 18 25 87 128 187 177 105 111 78]))))

(deftest get-formatted-data-test
  (testing "Creates a formatted datset with raw data"
    (is (= '({:day 1, :month 1, :year 1772, :temperature 32.0}
             {:day 1, :month 2, :year 1772, :temperature -15.0}
             {:day 1, :month 3, :year 1772, :temperature 18.0}
             {:day 1, :month 4, :year 1772, :temperature 25.0}
             {:day 1, :month 5, :year 1772, :temperature 87.0}
             {:day 1, :month 6, :year 1772, :temperature 128.0}
             {:day 1, :month 7, :year 1772, :temperature 187.0}
             {:day 1, :month 8, :year 1772, :temperature 177.0}
             {:day 1, :month 9, :year 1772, :temperature 105.0}
             {:day 1, :month 10, :year 1772, :temperature 111.0}
             {:day 1, :month 11, :year 1772, :temperature 78.0}
             {:day 1, :month 12, :year 1772, :temperature 112.0}
             {:day 2, :month 1, :year 1772, :temperature 20.0}
             {:day 2, :month 2, :year 1772, :temperature 7.0}
             {:day 2, :month 3, :year 1772, :temperature 28.0}
             {:day 2, :month 4, :year 1772, :temperature 38.0}
             {:day 2, :month 5, :year 1772, :temperature 77.0}
             {:day 2, :month 6, :year 1772, :temperature 138.0}
             {:day 2, :month 7, :year 1772, :temperature 154.0}
             {:day 2, :month 8, :year 1772, :temperature 158.0}
             {:day 2, :month 9, :year 1772, :temperature 143.0}
             {:day 2, :month 10, :year 1772, :temperature 150.0}
             {:day 2, :month 11, :year 1772, :temperature 85.0}
             {:day 2, :month 12, :year 1772, :temperature 62.0})
           (get-formatted-data '([1772 1 32 -15 18 25 87 128 187 177 105 111 78 112]
                                 [1772 2 20 7 28 38 77 138 154 158 143 150 85 62])))))
  (testing "that is invalid"
    (are [x] (thrown? AssertionError x)
      (get-formatted-data '('(1772 1 32 -15 18 25 87 128 187 177 105 111 78 112)))
      (get-formatted-data '(["foo"]))
      (get-formatted-data '(1772 1 32 -15 18 25 87 128 187 177 105 111 78 112)))))

(deftest lookup-month-name-test
  (testing "Month name lookup function"
    (is (= "January"
           (lookup-month-name 0)) "with a valid month number")
    (is (= nil
           (lookup-month-name -1))) "with an invalid month number"))

(deftest average-test
  (testing "Average mean calculation when given a collection"
    (is (= 4
           (average [2 4 6])))))

(deftest smallest-variation-test
  (testing "Smallest variation calculation when given a collection that each contain a key of temperature"
    (is (= '({:temperature 4})
           (smallest-variation 5 '({:temperature 1}
                                   {:temperature 2}
                                   {:temperature 3}
                                   {:temperature 4}
                                   {:temperature 7}))))))

(deftest greatest-variation-test
  (testing "Greatest variation calculation when given a collection that each contain a key of temperature"
    (is (= '({:temperature 1})
           (greatest-variation 5 '({:temperature 1}
                                   {:temperature 2}
                                   {:temperature 3}
                                   {:temperature 4}
                                   {:temperature 7}))))))

(def formatted-data-fixture '({:day 1, :month 1, :year 1772, :temperature 323.0}
                              {:day 29, :month 2, :year 1772, :temperature -151.0}
                              {:day 29, :month 2, :year 1776, :temperature 185.0}
                              {:day 1, :month 1, :year 2001, :temperature 252.0}
                              {:day 1, :month 5, :year 2002, :temperature 875.0}
                              {:day 1, :month 6, :year 1772, :temperature 1281.0}
                              {:day 1, :month 6, :year 1772, :temperature 1872.0}
                              {:day 1, :month 8, :year 1772, :temperature 1773.0}
                              {:day 1, :month 9, :year 1772, :temperature 1054.0}
                              {:day 1, :month 10, :year 1772, :temperature 1151.0}))

(def weather-data-year-fixture '({:year 1772, :temperature 1043.2857142857142}
                                 {:year 1776, :temperature 185.0}
                                 {:year 2001, :temperature 252.0}
                                 {:year 2002, :temperature 875.0}))

(def weather-data-month-fixture '({:month 1, :temperature 287.5}
                                  {:month 2, :temperature 17.0}
                                  {:month 5, :temperature 875.0}
                                  {:month 6, :temperature 1576.5}
                                  {:month 8, :temperature 1773.0}
                                  {:month 9, :temperature 1054.0}
                                  {:month 10, :temperature 1151.0}))

(def weather-data-month-year-fixture '({:month 9, :year 1772, :temperature 1054.0}
                                       {:month 2, :year 1772, :temperature -151.0}
                                       {:month 6, :year 1772, :temperature 1576.5}
                                       {:month 1, :year 1772, :temperature 323.0}
                                       {:month 8, :year 1772, :temperature 1773.0}
                                       {:month 5, :year 2002, :temperature 875.0}
                                       {:month 1, :year 2001, :temperature 252.0}
                                       {:month 2, :year 1776, :temperature 185.0}
                                       {:month 10, :year 1772, :temperature 1151.0}))


(deftest task4-questions-functions-test
  (testing "Each function to answer task 4's questions when passed with dummy formatted data"
    (is (= '("January 1 1772 323.0"
             "February 29 1776 185.0"
             "May 1 2002 875.0"
             "June 1 1772 1872.0"
             "August 1 1772 1773.0"
             "September 1 1772 1054.0"
             "October 1 1772 1151.0")
           (warmest-day-each-month formatted-data-fixture)) "for the warmest day each month")
    (is (= '({:year 1772, :temperature 1043.2857142857142}
             {:year 1776, :temperature 185.0}
             {:year 2001, :temperature 252.0}
             {:year 2002, :temperature 875.0})
           (mean-temp-each-year formatted-data-fixture)) "for the mean temperature for each year")
    (is (= "Warmest year was 1772 at a temperature of 1043.2857142857142 and Coldest year was 1776 at a temperature of 185.0"
           (warmest-and-coldest-year weather-data-year-fixture)) "for the warmest and coldest years")
    (is (= '({:month 1, :temperature 287.5}
             {:month 2, :temperature 17.0}
             {:month 5, :temperature 875.0}
             {:month 6, :temperature 1576.5}
             {:month 8, :temperature 1773.0}
             {:month 9, :temperature 1054.0}
             {:month 10, :temperature 1151.0})
           (mean-temp-each-month formatted-data-fixture)) "for the mean temperature for each month")
    (is (= '({:month 9, :year 1772, :temperature 1054.0}
             {:month 2, :year 1772, :temperature -151.0}
             {:month 6, :year 1772, :temperature 1576.5}
             {:month 1, :year 1772, :temperature 323.0}
             {:month 8, :year 1772, :temperature 1773.0}
             {:month 5, :year 2002, :temperature 875.0}
             {:month 1, :year 2001, :temperature 252.0}
             {:month 2, :year 1776, :temperature 185.0}
             {:month 10, :year 1772, :temperature 1151.0})
           (mean-temp-each-month-year formatted-data-fixture)) "for the mean temperature for each month for each year")
    (is (= '("Variation against January for Greatest: 1772 and Smallest: 1772"
             "Variation against February for Greatest: 1772 and Smallest: 1772"
             "Variation against May for Greatest: 2002 and Smallest: 2002"
             "Variation against June for Greatest: 1772 and Smallest: 1772"
             "Variation against August for Greatest: 1772 and Smallest: 1772"
             "Variation against September for Greatest: 1772 and Smallest: 1772"
             "Variation against October for Greatest: 1772 and Smallest: 1772")
           (greatest-and-smallest-variation weather-data-month-year-fixture
                                            weather-data-month-fixture)) "for the greatest and smallest variation against each month")
    (is (= "Warmest leap day was in 1776 at a temperature of 185.0"
           (warmest-leap-day formatted-data-fixture)) "for the warmest leap day")
    (is (= '({:century 18, :temperature 936.0}
             {:century 21, :temperature 563.5})
           (mean-temp-each-century formatted-data-fixture)) "for the mean temperature for each century"))
  (testing "that is invalid"
    (are [x] (thrown? AssertionError x)
      (warmest-day-each-month '({:foo 1.0}))
      (mean-temp-each-year '({:foo 1.0}))
      (warmest-and-coldest-year '({:foo 1.0}))
      (mean-temp-each-month '({:foo 1.0}))
      (mean-temp-each-month-year '({:foo 1.0}))
      (greatest-and-smallest-variation '({:foo 1.0}) '({:foo 1.0}))
      (warmest-leap-day '({:foo 1.0}))
      (mean-temp-each-century '({:foo 1.0})))))
