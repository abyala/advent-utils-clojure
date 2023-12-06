(ns abyala.advent-utils-clojure.math
  (:require [clojure.math :as m]))

(defn divisible? [num denom]
  (zero? (rem num denom)))

(defn signum
  "Given a number `n`, returns -1, 0, or 1 based on if the number is negative, zero, or positive."
  [n]
  (cond (zero? n) 0
        (neg? n) -1
        :else 1))

(defn quadratic-roots [a b c]
  (map (fn [op] (/ (op (- b) (m/sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
       [+ -]))