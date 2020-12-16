(ns day7.core-test
  (:require [clojure.test :refer :all]
            [day7.core :refer :all]))

(deftest input->dependency-map-test
  (testing "the keys are the container bags, and the keys are the containees with the quantity and colour"
    (is (= {"light red" [{:colour "bright white", :quantity 1}
                         {:colour "muted yellow", :quantity 2}]}
           (input->dependency-map {} "light red bags contain 1 bright white bag, 2 muted yellow bags."))))

  (testing "the bags that can't contain anything are not in the map"
    (is (= {}
           (input->dependency-map {} "faded blue bags contain no other bags.")))))
