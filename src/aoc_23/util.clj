(ns aoc-23.util
  (:require
   [clojure.java.io :as io]
   [clojure.math :refer [ceil]]))


(defn get-puzzle-input [n & flags]
  (let [test? (some #{:test} flags)]
    (-> (str "input/" (format "%02d" n) (when test? ".test") ".txt")
        io/resource
        slurp)))


(defn re-seq-pos
  "Find all matches of the given pattern in the given string and return
  them as a seq, including start and end positions. Credits to A. Webb,
  (https://stackoverflow.com/questions/21191045/get-string-indices-from-the-result-of-re-seq)."
  [p s]
  (let [m (re-matcher p s)]
    ((fn step []
       (when (.find m)
         (cons {:start (.start m), :end (.end m), :group (.group m)}
               (lazy-seq (step))))))))


(defn window-indices
  "Generate a lazy seq of all \"windows\" with the given `radius` around the elements of a
  vector or array with the given `length`, in the form `[i start end]`, end non-inclusive.
  
  Example:
  `1 3` -> `[0 0 2] [1 0 3] [2 1 3]`"
  [radius length]
  (for [i (range length)] [i (max (- i radius) 0) (min (+ i (inc radius)) length)]))


(defn exp [b e]
  (apply * (repeat e b)))


(defn parse-longs
  "Return a lazy seq of all the integers in the string `s`, e.g. `\"a 12 b 3.4\"` -> `(12 3 4)`"
  [s] (map parse-long (re-seq #"\d+" s)))


(defn merge-ranges
  "Given any number of half open integer intervals of the form `[start end]`
  (end non-inclusive), reduce them to a set of non-overlapping ranges.
  E.g. `[1 3] [5 7] [2 4]` -> `([1 4] [5 7])`"
  [& rs]
  (letfn [(-overlap? [r1 r2] (let [[r1 r2] (sort [r1 r2])] (> (r1 1) (r2 0))))
          (-merge [r1 r2] [(min (r1 0) (r2 0)) (max (r1 1) (r2 1))])]
    (sort
     (reduce
      (fn [acc r]
        (if (and (seq acc) (-overlap? r (first acc)))
          (conj (rest acc) (-merge r (first acc)))
          (conj acc r)))
      nil
      (sort (map vec rs))))))


(defn next-greater-int
  "Return the closest integer strictly greater than `x`."
  [x]
  (let [n (int (ceil x))]
    (if (= (int x) n) (inc n) n)))


(defn pad
  "Pad `coll` with `x` until it has `n` elements."
  [n x coll]
  (take n (concat coll (repeat x))))


(defn map-first
  "Return the result of applying `f` to the first element of `coll`
  cons'ed onto the rest of `coll`."
  [f coll]
  (when (seq coll) (cons (f (first coll)) (rest coll))))


(defn lcm [& xs]
  (reduce #(.divide (.multiply %1 %2) (.gcd %1 %2))
          (map biginteger xs)))
