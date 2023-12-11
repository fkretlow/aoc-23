(ns aoc.23.11
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [find-col-indices find-indices find-row-indices]]
   [aoc.lib.seq :refer [all-pairs]]
   [clojure.string :as str]))


(defn- find-galaxies
  "Given the universe as a two-dimensional matrix of characters, find the
  indices of all galaxies after inserting the given void-shift's worth of
  empty rows or columns next to each empty row or column."
  [universe void-shift]
  (let [is-empty? (fn [coll] (every? #(= \. %) coll))
        empty-rows (find-row-indices is-empty? universe)
        empty-cols (find-col-indices is-empty? universe)
        galaxies (find-indices #(= \# %) universe)]
    (map (fn [[i j]]
           [(+ (* (count (take-while #(< % i) empty-rows)) void-shift) i)
            (+ (* (count (take-while #(< % j) empty-cols)) void-shift) j)])
         galaxies)))


(defn- distance [[i j] [i' j']]
  (+ (abs (- i i')) (abs (- j j'))))


(defn part-1 [universe]
  (let [galaxies (find-galaxies universe 1)]
    (->> (all-pairs galaxies)
         (map #(apply distance %))
         (apply +))))


(defn part-2 [universe]
  (let [galaxies (find-galaxies universe 999999)]
    (->> (all-pairs galaxies)
         (map #(apply distance %))
         (apply +))))


(let [universe (map vec (str/split-lines (get-puzzle-input 23 11)))]
  (println "Part 1: " (part-1 universe))
  (println "Part 2: " (part-2 universe)))
