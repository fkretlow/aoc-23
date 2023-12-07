(ns aoc-23.01
  (:require
   [aoc-23.util :refer [get-puzzle-input]]
   [clojure.string :as str]))


(def ^:private number-literals
  (into {"zero" 0, "one" 1, "two" 2, "three" 3, "four" 4, "five" 5, "six" 6, "seven" 7, "eight" 8, "nine" 9}
        (for [n (range 10)] [(str n) n])))


(defn- find-first-and-last-occurence
  "Given any collection of string `literals`, return the ones that occur first
  and last in the given string `s` as a vector of two elements."
  [literals s]
  (let [joined-literals (str/join "|" literals)
        find-first #(second (re-find (re-pattern (format "^.*?(%s)" joined-literals)) %))
        find-last #(second (re-find (re-pattern (format "^.*(%s)" joined-literals)) %))]
    [(find-first s) (find-last s)]))


(defn- sum-up-calibration-values
  "Given a collection of acceptable number `literals`, return the sum of the calibration
  values for the given `lines.`"
  [literals lines]
  (->> (map (partial find-first-and-last-occurence literals) lines)
       (map (partial map number-literals))
       (map (fn [[x y]] (+ (* 10 x) y)))
       (apply +)))


(def part-1 (partial sum-up-calibration-values "0123456789"))
(def part-2 (partial sum-up-calibration-values (keys number-literals)))


(let [lines (-> (get-puzzle-input 1) (str/split-lines))]
  (println "Part 1: " (part-1 lines))
  (println "Part 2: " (part-2 lines)))
