(ns day5.core-test
  (:require [clojure.test :refer :all]
            [day5.core :refer :all]))

(deftest seat-id-test
  (testing "examples from the problem text"
    (is (= 567 (seat-id "BFFFBBFRRR")))
    (is (= 119 (seat-id "FFFBBBFRRR")))
    (is (= 820 (seat-id "BBFFBBFRLL")))))

(deftest seat-coordinates-test
  (testing "examples from the problem text"
    (is (= [44  5] (seat-coordinates "FBFBBFFRLR")))
    (is (= [70  7] (seat-coordinates "BFFFBBFRRR")))
    (is (= [14  7] (seat-coordinates "FFFBBBFRRR")))
    (is (= [102 4] (seat-coordinates "BBFFBBFRLL")))))

(deftest seat-row-test
  (testing "examples from the problem text"
    (is (= 44 (seat-row "FBFBBFFRLR")))
    (is (= 70 (seat-row "BFFFBBFRRR")))
    (is (= 14 (seat-row "FFFBBBFRRR")))
    (is (= 102 (seat-row "BBFFBBFRLL")))))

(deftest seat-col-test
  (testing "examples from the problem text"
    (is (= 5 (seat-col "FBFBBFFRLR")))
    (is (= 7 (seat-col "BFFFBBFRRR")))
    (is (= 7 (seat-col "FFFBBBFRRR")))
    (is (= 4 (seat-col "BBFFBBFRLL")))))
