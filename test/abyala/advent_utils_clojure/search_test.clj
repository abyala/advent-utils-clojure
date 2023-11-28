(ns abyala.advent-utils-clojure.search-test
  (:require [clojure.test :refer :all]
            [abyala.advent-utils-clojure.search :as s]))

(defn no-next-vals [_] nil)
(defn incr-vec [n] [(inc n)])
(defn mod-10? [n] (zero? (mod n 10)))

(deftest breadth-first-test
  (testing "Normal input"
    (are [input expected] (= expected (s/breadth-first input incr-vec mod-10?))
                          [6] 10
                          [5] 10
                          [10] 10
                          [5 6] 10
                          (list 5 6) 10))
  (testing "Scalar input"
    (are [input expected] (= expected (s/breadth-first input incr-vec mod-10?))
                          6 10
                          11 20))

  (testing "Nil next-vals-fn"
    (are [input] (nil? (s/breadth-first input no-next-vals mod-10?))
                 [5]
                 6)
    (are [input expected] (= expected (s/breadth-first input no-next-vals mod-10?))
                          (range) 0
                          (range 1 20) 10
                          [10] 10
                          (list 9 10 11) 10))

  (testing "Multiple values in next-vals-fn"
    (are [input expected] (= expected (s/breadth-first input #(vector (inc %) (+ 6 %)) mod-10?))
                          [10 4] 10
                          [9 4] 10
                          [8 4] 10)))

(deftest stateful-tests
  ; Take turns of going either L or R at an intersection. Return the first decision point after making 5 turns.
  ; Breadth-first: L, R, LL, LR, RL
  ; Depth-first: L, LL, LLL, LLLL, LLLLL
  (let [left-and-right [\L \R]
        next-turns (fn [path] (map #(str path %) left-and-right))]
    (are [alg expected] (= expected (alg [] left-and-right (fn [acc path]
                                                             (if (= (count acc) 5)
                                                               (s/done-searching (last acc))
                                                               (s/keep-searching (conj acc path) (next-turns path))))))
                        s/breadth-first-stateful "RL"
                        s/depth-first-stateful "LLLLL")))

(deftest depth-first-test
  (testing "Normal input"
    (are [input expected] (= expected (s/depth-first input incr-vec mod-10?))
                          [6] 10
                          [5] 10
                          [10] 10
                          [5 6] 10
                          (list 5 6) 10))
  (testing "Scalar input"
    (are [input expected] (= expected (s/depth-first input incr-vec mod-10?))
                          6 10
                          11 20))

  (testing "Using no-next-vals-fn"
    (are [input] (nil? (s/depth-first input no-next-vals mod-10?))
                 [5]
                 6)
    (are [input expected] (= expected (s/depth-first input no-next-vals mod-10?))
                          (range) 0
                          (range 1 20) 10
                          [10] 10
                          (list 9 10 11) 10))

  (testing "Multiple values in next-vals-fn"
    (are [input expected] (= expected (s/depth-first input #(vector (inc %) (+ 6 %)) mod-10?))
                          [10 4] 10
                          [9 4] 10
                          [8 4] 10)))

(deftest compare-algorithms
  (are [alg input expected] (= expected (alg input incr-vec mod-10?))
                            s/breadth-first [5 19] 20
                            s/depth-first [5 19] 10))