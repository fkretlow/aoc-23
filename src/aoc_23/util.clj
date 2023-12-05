(ns aoc-23.util
  (:require
   [clojure.java.io :as io]))


(defn get-puzzle-input [n & flags]
  (let [test? (some #{:test} flags)]
    (-> (str "input/" (format "%02d" n) (when test? ".test") ".txt")
        io/resource
        slurp)))


(defn merge-max
  "Given any number of maps whose values are comparable with `max`, merge them
  in such a way that the result contains all keys and if any two maps contain
  a key, it is mapped to the greater of the two values."
  [& ms]
  (reduce
   #(reduce (fn [m [k v]] (let [v (if (m k) (max (m k) v) v)] (assoc m k v))) %1 %2)
   (first ms)
   (next ms)))


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
  "Given the `length` of a vector, return a lazy seq of the indices of all \"windows\" 
  on the vector that contain the line at position `i` and, if they exist, the previous 
  and the next line, in the form `[i start end]`."
  [length]
  (for [i (range length)] [i (max (dec i) 0) (min (+ 2 i) length)]))


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
