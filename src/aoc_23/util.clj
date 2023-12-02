(ns aoc-23.util
  (:require
   [clojure.java.io :as io]))


(defn get-puzzle-input [n & flags]
  (let [test? (contains? (set flags) :test)]
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


