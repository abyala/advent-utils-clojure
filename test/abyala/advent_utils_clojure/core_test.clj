(ns abyala.advent-utils-clojure.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [abyala.advent-utils-clojure.core :as c]))

(deftest digit?-test
  (are [c] (c/digit? c)
           \0
           \1
           \9)
  (are [c] (not (c/digit? c))
           \space
           \A
           \-
           \.))

(deftest count-if-test
  (are [input expected] (= expected (c/count-if even? input))
                        nil 0
                        () 0
                        [] 0
                        [3] 0
                        [3 4 5] 1
                        (range 10) 5))

(deftest divisible-test
  (are [num den] (true? (c/divisible? num den))
                 9 3
                 3 3
                 3 1)
  (are [num den] (false? (c/divisible? num den))
                 9 4
                 3 2
                 2 3))

(deftest first-when-test
  (are [input] (nil? (c/first-when even? input))
               nil
               ()
               []
               (list 3)
               [3 5 7]
               (range 1 11 2))
  (are [input expected] (= expected (c/first-when even? input))
                        [2] 2
                        (range 10) 0
                        [3 4 5] 4))

(deftest index-of-first-test
  (are [input] (nil? (c/index-of-first even? input))
               nil
               ()
               []
               (list 3)
               [3 5 7]
               (range 1 11 2))
  (are [input expected] (= expected (c/index-of-first even? input))
                        [2] 0
                        (range 10) 0
                        [3 4 5] 1))

(deftest into-map-by-test
  (testing "Numeric values (square function)"
    (are [input expected] (= expected (c/into-map-by #(* % %) input))
                          nil {}
                          () {}
                          [5] {25 5}
                          (range 4) {0 0, 1 1, 4 2, 9 3}))

  (testing "String values (capitalize function)"
    (are [input expected] (= expected (c/into-map-by str/capitalize input))
                          nil {}
                          () {}
                          ["hello"] {"Hello" "hello"}
                          ["a" "b" "a" "B" "c"] {"A" "a", "B" "B", "C" "c"})))

(deftest map-add-test
  (are [m k v expected] (= expected (c/map-add m k v))
                        nil :a 1 {:a 1}
                        {} :a 1 {:a 1}
                        {:b 2} :a 1 {:a 1 :b 2}
                        {:a 0} :a 1 {:a 1}))

(deftest map-conj-test
  (testing "No supplier provided"
    (are [m k v expected] (= expected (c/map-conj m k v))
                          nil :a 1 {:a [1]}
                          {} :a 1 {:a [1]}
                          {:b 2} :a 1 {:a [1] :b 2}
                          {:a [0]} :a 1 {:a [0 1]}
                          {:a [1]} :a 1 {:a [1 1]}))
  (testing "Hash-set supplier provided"
    (are [m k v expected] (= expected (c/map-conj hash-set m k v))
                          nil :a 1 {:a #{1}}
                          {} :a 1 {:a #{1}}
                          {:b 2} :a 1 {:a #{1} :b 2}
                          {:a #{0}} :a 1 {:a #{0 1}}
                          {:a #{1}} :a 1 {:a #{1}})))

(deftest parse-binary-test
    (are [input expected] (= expected (c/parse-binary input))
                          "0" 0
                          "1" 1
                          "10" 2)
    (is (thrown? Exception (c/parse-binary nil)))
    (is (thrown? Exception (c/parse-binary "")))
    (is (thrown? Exception (c/parse-binary "3")))
    (is (thrown? Exception (c/parse-binary "five"))))

(deftest parse-int-char-test
  (are [input expected] (= expected (c/parse-int-char input))
                        \0 0
                        \1 1
                        \8 8)

  (is (thrown? Exception (c/parse-int-char nil)))
  (is (thrown? Exception (c/parse-int-char "")))
  (is (thrown? Exception (c/parse-int-char 4)))
  (is (thrown? Exception (c/parse-int-char \-)))
  (is (thrown? Exception (c/parse-int-char \space)))
  (is (thrown? Exception (c/parse-int-char \a))))

(deftest signum-test
  (are [input expected] (= expected (c/signum input))
                        0 0
                        1 1
                        5 1
                        -1 -1
                        -4 -1))

(deftest take-until-test
  (are [input expected] (= expected (c/take-until even? input))
                        nil ()
                        () ()
                        [] ()
                        [1 3 5] [1 3 5]
                        [1 3 5 6 7] (list 1 3 5 6)))
