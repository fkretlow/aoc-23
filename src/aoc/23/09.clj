(ns aoc.23.09
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]))


(defn- steps [xs]
  (->> (partition 2 1 xs)
       (mapv (fn [[a b]] (- b a)))))


(defn- extrapolate [values]
  (->> (iterate steps values)
       (take-while #(not (every? zero? %)))
       (map last)
       (apply +)))


(defn- extrapolate-backwards [values]
  (->> (iterate steps values)
       (take-while #(not (every? zero? %)))
       (map first)
       (reverse)
       (reduce #(- %2 %1))))


(defn part-1 [value-seqs] (apply + (map extrapolate value-seqs)))
(defn part-2 [value-seqs] (apply + (map extrapolate-backwards value-seqs)))


(let [value-seqs (->> (get-puzzle-input 23 9)
                      (str/split-lines)
                      (map parse-longs))]
  (println "Part 1: " (part-1 value-seqs))
  (println "Part 2: " (part-2 value-seqs)))
