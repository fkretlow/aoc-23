(ns aoc.23.16
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mget mshape v+]]
   [clojure.string :as str]))


(defn- horizontal? [d] (= 0 (nth d 0)))
(defn- vertical? [d] (= 0 (nth d 1)))
(defn- inside? [[h w] [i j]] (and (<= 0 i (dec h)) (<= 0 j (dec w))))


(defn- next-directions [shape d]
  (case shape
    \- (if (horizontal? d) [d] [[0 -1] [0 1]])
    \| (if (vertical? d) [d] [[-1 0] [1 0]])
    \\ (case d [-1 0] [[0 -1]], [0 -1] [[-1 0]], [1 0] [[0 1]], [0 1] [[1 0]])
    \/ (case d [-1 0] [[0 1]], [0 -1] [[1 0]], [1 0] [[0 -1]], [0 1] [[-1 0]])
    [d]))


(defn- next-steps [grid [p d]]
  (->> (map #(-> [(v+ p %) %]) (next-directions (mget grid p) d))
       (filter #(inside? (mshape grid) (first %)))))


(defn- count-energized [grid start]
  (loop [visited (transient #{})
         stack (list start)]
    (if-let [[p d] (first stack)]
      (if (visited [p d])
        (recur visited (pop stack))
        (recur (conj! visited [p d]) (apply conj (pop stack) (next-steps grid [p d]))))
      (count (into #{} (map first (persistent! visited)))))))


(defn part-1 [grid] (count-energized grid [[0 0] [0 1]]))


(defn part-2 [grid]
  (let [[h w] (mshape grid)]
    (apply max
           (concat
            (apply concat (for [i (range h)]
                            [(count-energized grid [[i 0] [0 1]])
                             (count-energized grid [[i (dec w)] [0 -1]])]))
            (apply concat (for [j (range w)]
                            [(count-energized grid [[0 j] [1 0]])
                             (count-energized grid [[(dec h) j] [-1 0]])]))))))


(let [grid (char-matrix (str/split-lines (get-puzzle-input 23 16)))]
  (println "Part 1: " (time (part-1 grid)))
  (println "Part 2: " (time (part-2 grid))))
