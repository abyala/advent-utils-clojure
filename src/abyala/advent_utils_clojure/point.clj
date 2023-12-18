(ns abyala.advent-utils-clojure.point
  (:require [clojure.string :as str]))

(def origin [0 0])
(def left [-1 0])
(def right [1 0])
(def up [0 1])
(def down [0 -1])
(def cardinal-directions [left right up down])

(defn parse-to-char-coords
  "Given an input string of a multi-line grid of single characters, returns a lazy sequence of [[x y] c] tuples of
  [x y] coords to each character c. If the function f is provided, it transforms each value c using that function."
  ([input] (parse-to-char-coords identity input))
  ([f input] (->> (str/split-lines input)
                  (map-indexed (fn [y line]
                                 (map-indexed (fn [x c] [[x y] (f c)]) line)))
                  (apply concat))))

(defn parse-to-char-coords-map
  "Given an input string of a multi-line grid of single characters, returns a map of {[x y] c} mapping the [x y]
  coordinates to each character c. If the function f is provided, it transforms each value c using that function."
  ([input] (parse-to-char-coords-map identity input))
  ([f input] (into {} (parse-to-char-coords f input))))

(defn horizontal-line?
  ([[point1 point2]] (horizontal-line? point1 point2))
  ([[_ y1] [_ y2]] (= y1 y2)))

(defn vertical-line?
  ([[point1 point2]] (vertical-line? point1 point2))
  ([[x1 _] [x2 _]] (= x1 x2)))

(defn neighbors [point]
  (map (partial mapv + point) [[0 1] [0 -1] [-1 0] [1 0]]))

(defn surrounding
  ([point] (surrounding false point))
  ([include-self? point] (let [points (if include-self? [[-1 -1] [0 -1] [1 -1] [-1 0] [0 0] [1 0] [-1 1] [0 1] [1 1]]
                                                        [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])]
                           (map (partial mapv + point) points))))

(defn perimeter-points [[x0 y0] [x1 y1]]
  (concat (for [x [x0 x1], y (range y0 (inc y1))] [x y])
          (for [y [y0 y1], x (range (inc x0) x1)] [x y])))

(defn touching? [[x0 y0] [x1 y1]]
  (and (<= (abs (- x0 x1)) 1)
       (<= (abs (- y0 y1)) 1)))

(defn manhattan-distance
  "Calculates the Manhattan/taxicab distance between two points."
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn bounding-box [points]
  (letfn [(min-max [nums] ((juxt (partial apply min) (partial apply max)) nums))]
    (let [[x-min x-max] (->> points (map first) min-max)
          [y-min y-max] (->> points (map second) min-max)]
      [[x-min y-min] [x-max y-max]])))

(defn- looped-pairs [vertices]
  (if (not= (first vertices) (last vertices))
    (recur (conj (vec vertices) (first vertices)))
    (partition 2 1 vertices)))

(defn perimeter-length
  "Takes a sequence of [x y] vertices and returns the length of the perimeter from walking in order from point to
  point. The vertices can either start and end with the same value, or else the function will close the path itself."
  [vertices]
  (transduce (map (partial apply manhattan-distance)) + (looped-pairs vertices)))

(defn polygon-area
  "Takes a sequence of [x y] vertices and returns the total area of the polygon. The vertices can either start and end
  with the same value, or else the function will close the polygon itself.

  This function makes use of the Shoelace Formula."
  ([vertices] (abs (/ (transduce (map (partial apply polygon-area)) + (looped-pairs vertices)) 2)))
  ([[x1 y1] [x2 y2]] (- (* x1 y2) (* x2 y1))))

(defn polygon-total-point-count
  "Calculates the total number of points that compose a polygon. The input `vertices` is a sequence of ordered points
   that define the perimeter of the polygon in a closed loop; if the vertices do not start and end with the same value,
   the function will close it. The output is the total number of points in the polygon, including both the perimeter
   and any internal points.

  This function makes use of Pick's Theorem."
  [vertices]
  (let [area (polygon-area vertices)
        perimeter (perimeter-length vertices)
        interior (- (inc area) (/ perimeter 2))]
    (+ perimeter interior)))

(defn polygon-interior-point-count
  "Calculates the total number of points that lie stricly inside a polygon. The input `vertices` is a sequence of
  ordered points that define the perimeter of the polygon in a closed loop; if the vertices do not start and end with
  the same value, the function will close it. The output is the number of points inside the polygon, excluding all
  points in the perimeter.

  This function makes use of Pick's Theorem."
  [vertices]
  (let [area (polygon-area vertices)]
    (if (pos? area)
      (- (inc area) (/ (perimeter-length vertices) 2))
      0)))
