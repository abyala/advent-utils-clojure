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

(deftest count-when-test
  (are [input expected] (= expected (c/count-when even? input))
                        nil 0
                        () 0
                        [] 0
                        [3] 0
                        [3 4 5] 1
                        (range 10) 5))

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

(deftest take-until-test
  (are [input expected] (= expected (c/take-until even? input))
                        nil ()
                        () ()
                        [] ()
                        [1 3 5] [1 3 5]
                        [1 3 5 6 7] (list 1 3 5 6)))

(deftest drop-until-test
  (are [input expected] (= expected (c/drop-until even? input))
                        nil ()
                        () ()
                        [] ()
                        [1 3 5] ()
                        [1 3 5 6 7] (list 7)
                        [1 3 5 6 7 8] (list 7 8)))

(deftest re-matcher-seq-test
  (are [pattern s expected] (= (c/re-matcher-seq pattern s) expected)
                            #"\d+" "abc" nil
                            #"\d+" "ab12c" [{:value "12", :start 2, :end 4}]
                            #"\w+" "Hello, world!" [{:value "Hello", :start 0, :end 5}
                                                    {:value "world", :start 7, :end 12}]))

(deftest split-longs-test
  (are [input expected] (= (c/split-longs input) expected)
                        "1" [1]
                        "12" [12]
                        "1 2" [1 2]
                        "abc" ()
                        "abc12de34f" [12 34]))

(deftest subs-to-end-test
  (testing "No end value"
    (are [s idx expected] (= (c/subs-to-end s idx) expected)
                          "Hello" 0 "Hello"
                          "Hello" 1 "ello"
                          "Hello" 4 "o"
                          "Hello" 5 ""
                          "" 0 ""
                          "" 1 ""))
  (testing "With end value"
    (are [s start end expected] (= (c/subs-to-end s start end) expected)
                                "Hello" 0 1 "H"
                                "Hello" 1 4 "ell"
                                "Hello" 1 5 "ello"
                                "Hello" 1 6 "ello"
                                "Hello" 5 1 ""
                                "" 0 2 ""
                                "" 1 2 "")))

(deftest repeat-string-test
  (are [n s expected] (= (c/repeat-string n s) expected)
                      0 "a" ""
                      1 "a" "a"
                      3 "a" "aaa"
                      3 :b ":b:b:b"
                      3 5 "555"))

(deftest unique-combinations-test
  (are [n coll expected] (= (c/unique-combinations n coll) expected)
                         1 nil ()
                         1 () ()
                         1 [] ()
                         2 nil ()
                         2 () ()
                         2 [] ()
                         1 [:a :b :c] [[:a] [:b] [:c]]
                         1 (list :a :b :c) [[:a] [:b] [:c]]
                         1 [[:a :b] 2 :something] [[[:a :b]] [2] [:something]]
                         2 [1 2 3] [[1 2] [1 3] [2 3]]
                         2 [[:a :b] [:c :d] :e] [[[:a :b] [:c :d]], [[:a :b] :e], [[:c :d] :e]]
                         3 [:a :b :c :d] [[:a :b :c] [:a :b :d] [:a :c :d] [:b :c :d]]
                         4 (list 1 2 3 4 5 6) [[1 2 3 4] [1 2 3 5] [1 2 3 6] [1 2 4 5] [1 2 4 6] [1 2 5 6]
                                               [1 3 4 5] [1 3 4 6] [1 3 5 6] [1 4 5 6]
                                               [2 3 4 5] [2 3 4 6] [2 3 5 6] [2 4 5 6]
                                               [3 4 5 6]])
  (are [coll expected] (= (c/unique-combinations coll) expected)
                       [:a :b :c] [[:a :b] [:a :c] [:b :c]]
                       (list 1) ()
                       (list 1 2) [[1 2]])

  (is (thrown? AssertionError (c/unique-combinations 0 [:a :b :c]))))

(deftest sum-test
  (testing "Single-argument"
    (are [coll expected] (= (c/sum coll) expected)
                         nil 0
                         [] 0
                         [0] 0
                         [1 2 3] 6
                         (range 5) 10
                         (list -1 0 3 -6) -4))
  (testing "Two-arguments"
    (are [f coll expected] (= (c/sum f coll) expected)
                           identity [1 2 3] 6
                           abs [1 -2 -3] 6
                           #(+ % %) [1 2 3] 12
                           parse-long ["1" "2" "-6"] -3)))

(deftest remove-each-subrange-test
  (testing "Single argument"
    (are [coll expected] (= (c/remove-each-subrange coll) expected)
                          nil ()
                          () ()
                          [] ()
                          [1] ()
                          (range 4) [[1 2 3] [0 2 3] [0 1 3] [0 1 2]]
                          [0 1 2 3] [[1 2 3] [0 2 3] [0 1 3] [0 1 2]]))
  (testing "Two arguments"
    (are [n coll expected] (= (c/remove-each-subrange n coll) expected)
                           1 nil ()
                           1 [] ()
                           1 [1] ()
                           1 (range 4) [[1 2 3] [0 2 3] [0 1 3] [0 1 2]]
                           2 (range 4) [[2 3] [0 3] [0 1]]
                           3 (range 4) [[3] [0]]
                           4 (range 4) ()
                           5 (range 4) ())
    (testing "Invalid removal size"
      (is (thrown? AssertionError (c/remove-each-subrange 0 (range 4)))))))

(deftest middle-test
  (testing "Only collection"
    (are [coll expected] (= (c/middle coll) expected)
                         nil nil
                         () nil
                         (list :a) :a
                         [:a] :a
                         [:a :b :c :d :e] :c
                         (list :a :b :c :d :e) :c)
    (is (thrown? IllegalArgumentException (c/middle [:a :b :c :d])))
    (is (thrown? IllegalArgumentException (c/middle (range 4))))
    (is (thrown? IllegalArgumentException (c/middle :fail [:a :b :c :d])))
    (is (thrown? IllegalArgumentException (c/middle :fail (range 4)))))
  (testing "Go low"
    (are [coll expected] (= (c/middle :low coll) expected)
                         nil nil
                         () nil
                         (list :a) :a
                         [:a] :a
                         [:a :b :c :d] :b
                         (range 4) 1))
  (testing "Go high"
    (are [coll expected] (= (c/middle :high coll) expected)
                         nil nil
                         () nil
                         (list :a) :a
                         [:a] :a
                         [:a :b :c :d] :c
                         (range 4) 2)))