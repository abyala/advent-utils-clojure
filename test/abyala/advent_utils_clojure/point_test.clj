(ns abyala.advent-utils-clojure.point-test
  (:require [clojure.test :refer :all]
            [abyala.advent-utils-clojure.point :as p]))

(deftest parse-to-char-coods-test
  (is (= (p/parse-to-char-coords "12\n34")
         (list [[0 0] \1] [[1 0] \2] [[0 1] \3] [[1 1] \4])))
  (is (= (p/parse-to-char-coords (comp parse-long str) "12\n34")
         (list [[0 0] 1] [[1 0] 2] [[0 1] 3] [[1 1] 4]))))

(deftest parse-to-char-coords-map-test
  (is (= (p/parse-to-char-coords-map "12\n34")
         {[0 0] \1, [1 0] \2, [0 1] \3, [1 1] \4}))
  (is (= (p/parse-to-char-coords-map (comp parse-long str) "12\n34")
         {[0 0] 1, [1 0] 2, [0 1] 3, [1 1] 4})))

(deftest horizontal-line-test
  (are [p1 p2] (true? (p/horizontal-line? p1 p2))
               [4 5] [4 5]
               [4 5] [9 5]
               [4 5] [1 5])
  (are [p1 p2] (false? (p/horizontal-line? p1 p2))
               [4 5] [4 6]
               [4 5] [3 3]
               [4 5] [4 2]))

(deftest vertical-line-test
  (are [p1 p2] (true? (p/vertical-line? p1 p2))
               [4 5] [4 5]
               [4 5] [4 9]
               [4 5] [4 0])
  (are [p1 p2] (false? (p/vertical-line? p1 p2))
               [4 5] [5 5]
               [4 5] [3 6]
               [4 5] [3 5]))

(deftest polygon-area-test
  ; Must use == instead of = because the type of polygon-area is a ratio, not a decimal
  (are [points expected] (== (p/polygon-area points) expected)
                         () 0
                         [[1 1]] 0
                         [[1 1] [2 1]] 0
                         [[1 1] [2 1] [2 2] [1 2]] 1
                         [[1 1] [1 2] [2 2] [2 1]] 1
                         [[1 1] [1 3] [3 3] [3 1]] 4
                         [[1 1] [3 1] [3 3] [1 3]] 4
                         [[1 1] [1 3] [3 3] [3 1] [1 1]] 4
                         [[1 1] [3 1] [3 3] [1 3] [1 1]] 4
                         [[1 6] [3 1] [7 2] [4 4] [8 5]] 16.5 ;Test from Wikipedia
                         [[1 6] [3 1] [7 2] [4 4] [8 5] [1 6]] 16.5))

(deftest perimeter-test
  (testing "Closed perimeter"                               ; First value in path matches the last
    (are [points expected] (= (p/perimeter points) expected)
                           () 0
                           [[1 1]] 0
                           [[1 1] [1 1]] 0
                           [[1 1] [3 1] [1 1]] 4
                           [[3 1] [1 1] [3 1]] 4
                           [[1 1] [4 2] [1 1]] 8
                           [[1 1] [3 1] [3 5] [1 5] [1 1]] 12
                           [[1 1] [1 5] [3 5] [3 1] [1 1]] 12))
  (testing "Unclosed perimeter"
    (are [points expected] (= (p/perimeter points) expected)
                           [[1 1] [3 1]] 4
                           [[3 1] [1 1]] 4
                           [[1 1] [4 2]] 8
                           [[1 1] [3 1] [3 5] [1 5]] 12
                           [[1 1] [1 5] [3 5] [3 1]] 12)))

(deftest total-points-within-path-test
  (are [points expected] (= (p/total-points-within-path points) expected)
                         [[1 1]] 1
                         [[1 1] [1 3]] 3
                         [[3 1] [1 1]] 3
                         [[1 1] [1 4] [5 4] [5 1]] 20
                         [[1 1] [5 1] [5 4] [1 4]] 20

                         ; Figure L with no interior points
                         [[0 0] [2 0] [2 1] [1 1] [1 3] [0 3]] 10

                         ; Looks like a dumbbell or a fat letter H
                         [[0 0] [0 3] [2 3] [2 2] [4 2] [4 3] [6 3] [6 0] [4 0] [4 1] [2 1] [2 0]] 26))