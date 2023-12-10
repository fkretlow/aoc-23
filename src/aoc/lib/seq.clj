(ns aoc.lib.seq)


(defn map-first
  "Return the result of applying `f` to the first element of `coll`
  cons'ed onto the rest of `coll`."
  [f coll]
  (when (seq coll) (cons (f (first coll)) (rest coll))))


(defn pad-end
  "Pad `coll` with `x` until it has `n` elements."
  [n x coll]
  (take n (concat coll (repeat x))))


(defn window-indices
  "Generate a lazy seq of all \"windows\" with the given `radius` around the elements of a
  vector or array with the given `length`, in the form `[i start end]`, end non-inclusive.
  
  Example:
  `1 3` -> `[0 0 2] [1 0 3] [2 1 3]`"
  [radius length]
  (for [i (range length)] [i (max (- i radius) 0) (min (+ i (inc radius)) length)]))

