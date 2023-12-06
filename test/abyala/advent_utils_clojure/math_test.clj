(ns abyala.advent-utils-clojure.math-test
  (:require [clojure.test :refer :all]
            [abyala.advent-utils-clojure.math :as m]))

(deftest divisible-test
  (are [num den] (true? (m/divisible? num den))
                 9 3
                 3 3
                 3 1)
  (are [num den] (false? (m/divisible? num den))
                 9 4
                 3 2
                 2 3))

(deftest signum-test
  (are [input expected] (= expected (m/signum input))
                        0 0
                        1 1
                        5 1
                        -1 -1
                        -4 -1))

(deftest quadratic-roots-test
  (are [a b c v1 v2] (= (set (m/quadratic-roots a b c)) #{v1 v2})
                     1 3 -4 -4.0 1.0
                     6 -2 0 0.0 (/ 1.0 3)))