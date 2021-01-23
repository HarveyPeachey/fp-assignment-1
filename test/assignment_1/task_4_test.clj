(ns assignment-1.task_4-test
  (:require [clojure.test :refer :all]
            [assignment-1.task_4 :refer :all]))

(deftest get-data-test
  (testing "Retrieving data with the get-data function"
    (is (= "caught exception: java.io.FileNotFoundException: https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.da"
           (get-data "https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.da" 14 #"\s+")) "With an invalid slurp url")
    (is (= [1772 1 32 -15 18 25 87 128 187 177 105 111 78 112]
           (first (get-data)))) "With the valid default slurp url"))

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

(deftest lookup-month-name-test)

(deftest average-test)

(deftest smallest-variation-test)

(deftest greatest-variation-test)

(deftest task4-data-assertions)
