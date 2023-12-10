(ns aoc.23.04
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.set :refer [intersection]]
   [clojure.string :as str])
  (:import
   [java.lang Math]))


(defn- parse-number-set [number-string]
  (->> (re-seq #"\d+" number-string)
       (map parse-long)
       (set)))


(defn- parse-and-evaluate-card [line]
  (->> (next (str/split line #"[:|]"))
       (map parse-number-set)
       (apply intersection)
       (count)))


(defn part-1 [scores]
  (->> (remove zero? scores)
       (map #(int (Math/pow 2 (dec %))))
       (apply +)))


(defn part-2 [scores]
  (loop [acc 0
         counts-and-scores (map vector (repeat 1) scores)]
    (if-let [[n score] (first counts-and-scores)]
      (recur (+ n acc)
             (map #(%1 %2)
                  (concat (repeat score #(update % 0 (partial + n))) (repeat identity))
                  (rest counts-and-scores)))
      acc)))


(let [scores (->> (get-puzzle-input 23 4)
                  (str/split-lines)
                  (map parse-and-evaluate-card))]
  (println "Part 1: " (part-1 scores))
  (println "Part 2: " (part-2 scores)))
