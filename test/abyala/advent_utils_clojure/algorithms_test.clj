(ns abyala.advent-utils-clojure.algorithms-test
  (:require [clojure.test :refer :all]
            [abyala.advent-utils-clojure.algorithms :as a]))

(def incr-one-fn #(vector (inc %)))
(def mod-10? #(zero? (mod % 10)))

(deftest breadth-first-test
  (testing "Normal input"
    (are [input expected] (= expected (a/breadth-first input incr-one-fn mod-10?))
                          [6] 10
                          [5] 10
                          [10] 10
                          [5 6] 10
                          (list 5 6) 10))
  (testing "Scalar input"
    (are [input expected] (= expected (a/breadth-first input incr-one-fn mod-10?))
                          6 10
                          11 20))
  (testing "No next-vals-fn"
    (are [input] (nil? (a/breadth-first input nil mod-10?))
                 [5]
                 6)
    (are [input expected] (= expected (a/breadth-first input mod-10?))
                          (range) 0
                          (range 1 20) 10
                          [10] 10
                          (list 9 10 11) 10))

  (testing "Nil next-vals-fn"
    (are [input] (nil? (a/breadth-first input nil mod-10?))
                 [5]
                 6)
    (are [input expected] (= expected (a/breadth-first input nil mod-10?))
                          (range) 0
                          (range 1 20) 10
                          [10] 10
                          (list 9 10 11) 10))

  (testing "Multiple values in next-vals-fn"
    (are [input expected] (= expected (a/breadth-first input #(vector (inc %) (+ 6 %)) mod-10?))
                          [10 4] 10
                          [9 4] 10
                          [8 4] 10)))

(deftest depth-first-test
  (testing "Normal input"
    (are [input expected] (= expected (a/depth-first input incr-one-fn mod-10?))
                          [6] 10
                          [5] 10
                          [10] 10
                          [5 6] 10
                          (list 5 6) 10))
  (testing "Scalar input"
    (are [input expected] (= expected (a/depth-first input incr-one-fn mod-10?))
                          6 10
                          11 20))

  (testing "Nil next-vals-fn"
    (are [input] (nil? (a/depth-first input nil mod-10?))
                 [5]
                 6)
    (are [input expected] (= expected (a/depth-first input nil mod-10?))
                          (range) 0
                          (range 1 20) 10
                          [10] 10
                          (list 9 10 11) 10))

  (testing "Multiple values in next-vals-fn"
    (are [input expected] (= expected (a/depth-first input #(vector (inc %) (+ 6 %)) mod-10?))
                          [10 4] 10
                          [9 4] 10
                          [8 4] 10)))

(deftest compare-algorithms
  (are [alg input expected] (= expected (alg input incr-one-fn mod-10?))
                            a/breadth-first [5 19] 20
                            a/depth-first [5 19] 10))