(ns aoc.23.18
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.math :refer [shoelace]]
   [aoc.lib.matrix :refer [v*scalar v+]]
   [clojure.string :as str]))


(def ^:private right [0 1])
(def ^:private left [0 -1])
(def ^:private up [-1 0])
(def ^:private down [1 0])


(defn- ->segment [line]
  (let [[_ d n] (re-find #"^([ULDR]) (\d+)" line)]
    [(case d "U" up, "L" left, "D" down, "R" right)
     (parse-long n)]))


(defn- ->hex-segment [line]
  (let [s (re-find #"#[0-9a-f]{6}" line)]
    [(case (last s) \0 right, \1 down, \2 left, \3 up)
     (Integer/parseInt (subs s 1 6) 16)]))


(defn solve [parser lines]
  (let [segments (map parser lines)
        path (reduce
              (fn [corners [v n]]
                (cons (v+ (or (first corners) [0 0]) (v*scalar v n)) corners))
              nil
              segments)
        area (shoelace path)
        corners (count path)
        straight-segments (- (apply + (map second segments)) corners)]
    (+ area (/ (+ corners straight-segments) 2.0) 1)))


(let [lines (str/split-lines (get-puzzle-input 23 18))]
  (println "Part 1:" (time (solve ->segment lines)))
  (println "Part 2:" (time (solve ->hex-segment lines))))
