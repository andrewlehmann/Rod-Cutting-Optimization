(ns pipe-optimization.core-test
  (:require [clojure.test :refer :all]
            [pipe-optimization.core :refer :all]
            [clojure.math.combinatorics :as combo]))

(deftest a-test
  (testing "canary"
    (is true)))

(deftest get-cuts-test
  (testing "get cuts for 3"
    (is (= [[1 2][1 1 1][3]] (get-cuts 3 {1 1, 2 2, 3 3})))))

(deftest max-price-for-0inch-test
  (testing "a 0 inch pipe should return 0 as its price"
    (is (= {:max-price 0 :cuts [[]]} (max-price 0 {1 1, 2 2, 3 3})))))

(deftest max-price-for-1inch-test
  (testing "a 1 inch pipe should return 1 as its price"
    (is (= {:max-price 1 :cuts [[1]]} (max-price 1 {1 1, 2 2, 3 3})))))

(deftest max-price-for-2inch-test
  (testing "a 2 inch pipe should return 2 as its price"
    (is (= {:max-price 2 :cuts [[1 1] [2]]} (max-price 2 {1 1, 2 2, 3 3})))))

(deftest max-price-for-3inch-test
  (testing "a 3 inch pipe should return 3 as its price"
    (is (= {:max-price 3 :cuts [[1 2][1 1 1][3]]} (max-price 3 {1 1, 2 2, 3 3})))))

(deftest max-price-for-4inch-test
  (testing "a 4 inch pipe should return 4 as its price"
    (is (= {:max-price 4 :cuts [[1 3][1 1 2][1 1 1 1][2 2]]} (max-price 4 {1 1, 2 2, 3 3, 4 2})))))

(deftest max-price-for-17inch-test
  (testing "a 17 inch pipe should return 8 different cuts"
    (let [results (max-price 17 {1 1, 2 2, 3 3})]
      (is (and 
            (= 33 (count (results :cuts)))
            (= 17 (results :max-price)))))))
