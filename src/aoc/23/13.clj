(ns aoc.23.13
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [transpose]]
   [aoc.lib.seq :refer [find-first]]
   [clojure.string :as str]))


(def patterns
  [["#.##..##."
    "..#.##.#."
    "##......#"
    "##......#"
    "..#.##.#."
    "..##..##."
    "#.#.##.#."]
   ["#...##..#"
    "#....#..#"
    "..##..###"
    "#####.##."
    "#####.##."
    "..##..###"
    "#....#..#"]])


(defn- is-axis? [pattern j]
  (loop [i (dec j), j j]
    (cond
      (or (< i 0) (<= (count pattern) j)) true
      (= (nth pattern i) (nth pattern j)) (recur (dec i) (inc j))
      :else false)))


(defn- symmetry-axis [pattern]
  (find-first (partial is-axis? pattern) (range 1 (count pattern))))


(defn- find-symmetry [pattern]
  (if-let [i (symmetry-axis pattern)]
    [:horizontal i]
    (when-let [j (symmetry-axis (mapv #(apply str %) (transpose pattern)))]
      [:vertical j])))


(defn part-1 [patterns]
  (->> (map find-symmetry patterns)
       (map (fn [[dir i]] (case dir :horizontal (* 100 i), :vertical i)))
       (apply +)))


(let [patterns (map str/split-lines (str/split (get-puzzle-input 23 13) #"\n\n"))]
  (println "Part 1: " (part-1 patterns)))
