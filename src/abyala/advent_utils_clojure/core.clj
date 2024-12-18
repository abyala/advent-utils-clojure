(ns abyala.advent-utils-clojure.core
  (:require [clojure.string :as str]))

(def block-char \█)

(defn digit?
  "Convenience wrapper for Java's static `Character.isDigit` method."
  [^Character c]
  (Character/isDigit c))

(defn parse-binary
  "Parses a string that represents a binary number into its decimal value."
  [s]
  (Long/parseLong s 2))

(defn parse-int-char
  "Parses a numeric character into its integer value, assuming base 10."
  [c]
  (let [n (- (int c) 48)]
    (if (<= 0 n 9)
      n
      (throw (IllegalArgumentException. (str "Input character '" c "' is not numeric."))))))

(defn split-by-blank-lines
  "Given an input string, returns a sequence of sub-strings, separated by a completely
  blank string. This function preserves any newlines between blank lines, and it filters
  out Windows' \"\r\" characters."
  [input]
  (-> (str/replace input "\r" "")
      (str/split #"\n\n")))

(defn split-blank-line-groups
  "Given an input string that represents multiple lines that get grouped together with blank line separators,
  returns a sequence of the line groups. Each line within a line group can optionally have a transformation
  function applied to it before being returned."
  ([input] (split-blank-line-groups identity input))
  ([xf input] (->> (str/split-lines input)
                   (partition-by str/blank?)
                   (take-nth 2)
                   (map (partial map xf)))))

(defn only-when
  "Returns v if (f v) is truthy, or else returns nil."
  [f v]
  (when (f v) v))

(defn split-longs
  "Given an input string, returns a sequence of all numbers extracted, coerced into longs. Any delimiter is acceptable,
  including whitespace, symbols, or any non-numeric character."
  [input]
  (map parse-long (re-seq #"-?\d+" input)))

(defn count-when
  "Returns the number of items in a collection that return a truthy response to a predicate filter."
  [pred coll]
  (count (filter pred coll)))

(defn index-of-first
  "Returns the index of the first value in a collection that returns a truthy response to a predicate filter."
  [pred coll]
  (first (keep-indexed #(when (pred %2) %1) coll)))

(defn take-until
  "Returns a lazy sequence of successive items from coll while `(pred item)` returns logical `true`,
  plus the first value that does not. Returns a transducer when no collection is provided."
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (rf result input)
          (reduced result))))))
  ([pred coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (if (pred (first s))
         (list (first s))
         (cons (first s) (take-until pred (rest s))))))))

(defn drop-until
  "Returns a lazy sequence of the items in coll starting one value after the first item for which (pred item) returns
   logical `true`."
  [pred coll]
  (cond
    (empty? coll) ()
    (pred (first coll)) (rest coll)
    :else (recur pred (rest coll))))

(defn first-when [pred coll]
  (first (filter pred coll)))

(defn into-map-by [f coll]
  (reduce #(assoc %1 (f %2) %2) {} coll))

(defn update-values
  "Thank you to Jay Fields' post for this awesome way to apply a function
  to every element of a map.
  http://blog.jayfields.com/2011/08/clojure-apply-function-to-each-value-of.html"
  [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn map-add
  "Inserts a key-value into map `m` if key `k` doesn't exist, or adds `v` to the existing value if it does."
  [m k v]
  (if ((or m {}) k)
    (update m k + v)
    (assoc m k v)))

(defn map-conj
  "Inserts a collection of `v` into map `m` if key `k` doesn't exist, or calls `conj` on the existing collection if
  it does. An optional supplier `supp` may be used for constructing the new collection, or else `vector` is used."
  ([m k v] (map-conj vector m k v))
  ([supp m k v] (if ((or m {}) k)
                  (update m k conj v)
                  (assoc m k (supp v)))))

(defn re-matcher-seq
  "Returns a lazy sequence of maps from applying a regular expression `re` to the string `s`. Each returned map
  will be of form `{:value v, :start x, :end y}` where `:value` is the text value from the captured group, and
  `:start` and `:end` are the start and end indexes of that group's characters."
  [re s]
  (letfn [(next-value [m] (when (.find m)
                            (cons {:value (.group m), :start (.start m), :end (.end m)}
                                  (lazy-seq (next-value m)))))]
    (next-value (re-matcher re s))))

(defn subs-to-end
  "Returns the substring of s beginning at start inclusive, and ending at end (defaulting to length of string)
  exclusive. Effectively the same as Clojure's built-in subs function, but it terminates at the end of the string
  instead of throwing a StringIndexOutOfBoundsException."
  ([s start] (if (< start (count s)) (subs s start) ""))
  ([s start end] (if (< start (count s))
                   (subs s start (min end (count s)))
                   "")))

(defn repeat-string
  "Creates a string by repeating a value `s` a number of times."
  [n s]
  (apply str (repeat n s)))

(defn unique-combinations
  "Takes a collection of values and returns a vector of all unique combination of `n` elements in the collection.
  Since ordering does not matter in a combination, calling `(unique-combinations [:a :b])` returns `([:a :b]) ` and
  not `([:a :b] [:b :a])`. `n` must be a positive integer for the number of elements in each collection to return,
  with a default value of 2 for unique pairs."
  ([coll] (unique-combinations 2 coll))
  ([n coll]
   {:pre [(pos-int? n)]}
   (cond
     (nil? coll) ()
     (empty? coll) ()
     (not (indexed? coll)) (recur n (vec coll))
     (= n 1) (map vector coll)
     (= n 2) (let [size (count coll)]
               (for [v1 (range size)
                     v2 (range (inc v1) size)]
                 [(coll v1) (coll v2)]))
     :else (mapcat (fn [idx] (keep (fn [nested] (when (seq nested) (apply conj [(coll idx)] nested)))
                                   (unique-combinations (dec n) (vec (drop (inc idx) coll)))))
                   (range (count coll))))))

(defn sum
  "Sums the values in a collection. If a function `f` is provided, then map `f` to each value in the collection
  before adding them together."
  ([coll] (apply + coll))
  ([f coll] (transduce (map f) + coll)))

(defn remove-each-subrange
  "Returns the result of every way to remove a subrange from a collection, defaulting to a removal length of 1.
  The length must be a positive integer or it will fail with an assertion error. The function will return an empty
  list if the value of `n` is at least the size of the input collection."
  ([coll] (remove-each-subrange 1 coll))
  ([n coll]
   {:pre [(pos-int? n)]}
   (if (< n (count coll))
     (map #(concat (take % coll) (drop (+ n %) coll))
          (range (- (count coll) n -1)))
     ())))

(defn middle
  "Returns the value in the middle of a collection. If given an empty collection, it returns nil. If given a collection
  with an odd number of values, it picks the middle one. If given a collection with an even number of values, it
  decides based on the value of the `middle-strategy` argument, which accepts values of `:low`, `:high` or `:fail`,
  where `:fail` (the default) throws an IllegalArgumentException."
  ([coll] (middle :fail coll))
  ([middle-strategy coll]
   {:pre [(#{:low :high :fail} middle-strategy)]}
   (when (seq coll)
     (let [c (count coll)
           idx (quot c 2)]
       (cond (or (odd? c) (= middle-strategy :high)) (nth coll idx)
             (= middle-strategy :low) (nth coll (dec idx))
             :else (throw (IllegalArgumentException.
                            "Function \"middle\" cannot be called with an even-length collection without a middle-strategy.")))))))