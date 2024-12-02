(ns aoc.24.02
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]))


(def ^:private test-input
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")


(defn- parse-lists [input]
  (map parse-longs (str/split-lines input)))


(defn- is-safe-step? [dir? [a b]]
  (let [d (- b a)] (and (dir? d) (< 0 (abs d) 4))))


(defn- is-safe? [[fst snd, :as report]]
  (let [dir? (cond (< fst snd) pos?, (> fst snd) neg?, :else zero?)]
    (->> (partition 2 1 report)
         (map (partial is-safe-step? dir?))
         (every? true?))))


(defn part-1 [reports]
  (count (filter is-safe? reports)))


(comment (let [reports (parse-lists test-input)]
           (map is-safe? reports)))


(let [reports (parse-lists (get-puzzle-input 24 2))]
  (println "Part 1:" (part-1 reports)))
