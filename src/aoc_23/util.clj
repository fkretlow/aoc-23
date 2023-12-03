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
