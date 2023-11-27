(ns abyala.advent-utils-clojure.algorithms)

(defn- no-next-vals-fn [_] nil)

(defn breadth-first
  "Performs a breadth first search, returning the first value that satisfies the predicate `pred`. The function will
  go through the values in `initial-vals` in order. For each value that fails the `pred`, the optional `next-vals-fn`
  will be invoked to identify additional values to add to the end of the initial-vals queue."

  ([initial-vals pred]
   (breadth-first initial-vals no-next-vals-fn pred))

  ([initial-vals next-vals-fn pred]
   (cond
     ; Validate initial-vals
     (nil? initial-vals) nil
     (not (coll? initial-vals)) (recur (list initial-vals) next-vals-fn pred)

     ; Validate next-vals-fn
     (nil? next-vals-fn) (recur initial-vals no-next-vals-fn pred)

     ; Good to go
     :else (loop [vals (seq initial-vals)]
             (when (seq vals)
               (let [[v & v'] vals]
                 (if (pred v)
                   v
                   (recur (concat v' (next-vals-fn v))))))))))

(defn depth-first [initial-vals next-vals-fn pred]
  (cond
    ;Validate initial-vals
    (nil? initial-vals) nil
    (not (coll? initial-vals)) (recur (list initial-vals) next-vals-fn pred)

    ;Validate next-vals-fn
    (nil? next-vals-fn) (recur initial-vals no-next-vals-fn pred)

    ; Good to go
    :else (loop [vals (seq initial-vals)]
            (when (seq vals)
              (let [[v & v'] vals]
                (if (pred v)
                  v
                  (recur (concat (next-vals-fn v) v'))))))))