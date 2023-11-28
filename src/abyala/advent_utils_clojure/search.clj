(ns abyala.advent-utils-clojure.search)

(defn- breadth-first-append-values [old-vals new-vals]
  (concat old-vals new-vals))
(defn- depth-first-append-values [old-vals new-vals]
  (concat new-vals old-vals))

(defn- stateless-search [append-vals-fn initial-vals next-vals-fn pred]
  (cond
    ; Validate initial-vals
    (nil? initial-vals) nil
    (not (coll? initial-vals)) (recur append-vals-fn (list initial-vals) next-vals-fn pred)

    ; Good to go
    :else (loop [vals (seq initial-vals)]
            (when (seq vals)
              (let [[v & v'] vals]
                (if (pred v)
                  v
                  (recur (append-vals-fn v' (next-vals-fn v)))))))))

(defn breadth-first
  "Performs a breadth first search, returning the first value that satisfies the predicate `pred`. The function will
  go through the values in `initial-vals` in order. For each value that fails the `pred`, the `next-vals-fn`
  will be invoked to identify additional values, if any, to add to the end of the initial-vals queue. `next-vals-fn`
  is expected to take in a single argument for the current value being evaluated."

  [initial-vals next-vals-fn pred]
  (stateless-search breadth-first-append-values initial-vals next-vals-fn pred))


(defn depth-first
  "Performs a depth-first search, returning the first value that satisfies the predicate `pred`. The function will
  go through the values in `initial-vals` in order. For each value that fails the `pred`, the `next-vals-fn`
  will be invoked to identify additional values, if any, to add to the front of the initial-vals queue. `next-vals-fn`
  is expected to take in a single argument for the current value being evaluated."

  [initial-vals next-vals-fn pred]
  (stateless-search depth-first-append-values initial-vals next-vals-fn pred))

(defrecord DoneSearching [value])
(defrecord KeepSearching [next-state next-vals])

(defn done-searching
  "Invoked within the `eval-fn` of a stateful search to demonstrate that the search has found its result, `v`."
  [v] (->DoneSearching v))
(defn keep-searching
  "Invoked within the `eval-fn` of a stateful search to demonstrate that the search must continue. The `next-state`
  argument is the state to pass to the next value being evaluated. The optional `next-vals` returns a collection of
  values to add to the search space."
  ([next-state] (->KeepSearching next-state ()))
  ([next-state next-vals]
   (assert ((some-fn nil? coll?) next-vals) "next-vals must be either nil or a collection")
   (->KeepSearching next-state next-vals)))

(defn- stateful-search [append-vals-fn initial-state initial-vals eval-fn]
  (loop [vals (seq initial-vals), state initial-state]
    (when (seq vals)
      (let [[v & v'] vals
            result (eval-fn state v)]
        (cond
          (instance? DoneSearching result) (:value result)
          (instance? KeepSearching result) (recur (append-vals-fn v' (:next-vals result))
                                                  (:next-state result))
          :else (throw (IllegalArgumentException. "Evaluation function must return either DoneSearching or KeepSearching")))))))

(defn breadth-first-stateful
  "Similar to [[breadth-first]], this performs a breadth-first search but retains state between evaluations of searched
  values. The `eval-fn` must take two arguments, the `state` and the `value` being considered, and its return must
  call either [[done-searching]] or [[keep-searching]]. "
  [initial-state initial-vals eval-fn]
  (stateful-search breadth-first-append-values initial-state initial-vals eval-fn))

(defn depth-first-stateful
  "Similar to [[depth-first]], this performs a depth-first search but retains state between evaluations of searched
  values. The `eval-fn` must take two arguments, the `state` and the `value` being considered, and its return must
  call either [[done-searching]] or [[keep-searching]]. "
  [initial-state initial-vals eval-fn]
  (stateful-search depth-first-append-values initial-state initial-vals eval-fn))
