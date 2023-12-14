(ns aoc.23.14
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :as m]
   [clojure.string :as str]))


(defn- transpose [platform] (map #(apply str %) (m/transpose platform)))


(defn- line-segments
  "Split a line into separate segments of either cube-shaped rocks 
  or rounded rocks and empty space.
  Example: '#..O##O...#..' -> ('#' '..O' '##' 'O...' '#' '..')"
  [line]
  (re-seq #"#+|[.O]+" line))


(defn- tilt-segment
  "Given a line segment, shift all the rocks as far to the left or right as possible.
  Example: :left '..O.O' -> 'OO...'"
  [direction segment]
  (if (str/starts-with? segment "#")
    segment
    (let [rocks (count (re-seq #"O" segment))]
      (case direction
        :left (apply str (concat (repeat rocks \O) (repeat (- (count segment) rocks) \.)))
        :right (apply str (concat (repeat (- (count segment) rocks) \.) (repeat rocks \O)))))))


(defn- tilt-line 
  "Given a whole line, shift all rounded rocks as far to the left or right as possible.
  Example: :left '.O##..O' -> 'O.##O..'"
  [direction line]
  (->> (line-segments line)
       (map (partial tilt-segment direction))
       (apply str)))


(defn- tilt-platform 
  "Tilt the platform in the given direction and shift all rounded rocks accordingly."
  [direction platform]
  (case direction
    :north (transpose (map (partial tilt-line :left) (transpose platform)))
    :west (map (partial tilt-line :left) platform)
    :south (transpose (map (partial tilt-line :right) (transpose platform)))
    :east (map (partial tilt-line :right) platform)))


(defn northern-rock-weight 
  "Compute the weight applied to the northern support beams."
  [platform]
  (->> (map #(count (re-seq #"O" %)) platform)
       (reverse)
       (map-indexed (fn [i n] (* (inc i) n)))
       (apply +)))


(defn part-1 [platform]
  (northern-rock-weight (tilt-platform :north platform)))


(defn apply-tilting-cycle 
  "Tilt the platform north, west, south, and east and return its resulting
  rock layout."
  [platform]
  (reduce #(tilt-platform %2 %1) platform [:north :west :south :east]))


(def ^:private max-tries 100000)

(defn find-cycle-in-iteration
  "Find out if there are non-negative integers m < n such that f^m(x) = f^n(x)
  for the given x. Return nil if no cycle was found after max-tries iterations."
  [f x]
  (loop [x x
         seen-at {x 0}
         n 1]
    (when (< n max-tries)
      (let [x' (f x)]
        (if-let [m (get seen-at x')]
          [m n]
          (recur x' (assoc seen-at x' n) (inc n)))))))


(defn part-2 [platform iterations]
  (let [[m n] (find-cycle-in-iteration apply-tilting-cycle platform)
        iterations' (+ m (rem (- iterations m) (- n m)))
        platform' (nth (iterate apply-tilting-cycle platform) iterations')]
    (northern-rock-weight platform')))


(let [platform (str/split-lines (get-puzzle-input 23 14))]
  (println "Part 1: " (part-1 platform))
  (println "Part 2: " (part-2 platform 1000000000)))
