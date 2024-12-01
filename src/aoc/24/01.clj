(ns aoc.24.01
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.seq :refer [count-unique]]
   [clojure.string :as str]))


(defn- parse-lists [input]
  (->> (str/split-lines input)
       (map parse-longs)
       (apply map vector)))


(defn part-1 [l1 l2]
  (let [l1' (sort l1)
        l2' (sort l2)]
    (->> (map vector l1' l2')
         (map #(apply - %))
         (map abs)
         (apply +))))


(defn part-2 [l1 l2]
  (let [counts (count-unique l2)]
    (->> l1
         (map (fn [x] (* x (get counts x 0))))
         (apply +))))


(let [[l1 l2] (parse-lists (get-puzzle-input 24 1))]
  (println "Part 1:" (part-1 l1 l2))
  (println "Part 2:" (part-2 l1 l2)))
