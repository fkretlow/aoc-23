(ns aoc.23.10
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.math :refer [shoelace]]
   [aoc.lib.matrix :refer [char-matrix mfind mget mshape v*scalar v+ v-]]
   [aoc.lib.seq :refer [find-first]]
   [clojure.string :as str]))


(def ^:private north [-1 0])
(def ^:private west [0 -1])
(def ^:private south [1 0])
(def ^:private east [0 1])
(def ^:private shape-connections {\| #{north south}
                                  \- #{west east}
                                  \L #{north east}
                                  \J #{north west}
                                  \7 #{south west}
                                  \F #{south east}
                                  \. #{}
                                  \S #{north west south east}})


(defn- inside? [[h w] [i j]] (and (<= 0 i (dec h)) (<= 0 j (dec w))))


(defn- connect?
  "Does the pipe shape at p1 on the grid connect to p2?"
  [grid p1 p2]
  (let [d (v- p2 p1)]
    (boolean (and (inside? (mshape grid) p2)
                  (#{north west south east} d)
                  ((get shape-connections (mget grid p1)) d)
                  ((get shape-connections (mget grid p2)) (v*scalar d -1))))))


(defn- connected-tiles
  "Return all tiles that are connected to the tile at p on the grid."
  [grid p]
  (for [d (shape-connections (mget grid p))
        :let [p' (v+ p d)]
        :when (connect? grid p p')]
    p'))


(defn- next-tile
  "Given the path as a 'stack' of last visited positions, return the next position."
  [grid [cur prev]]
  (find-first #(not= prev %) (connected-tiles grid cur)))


(defn- find-loop
  "Find the one loop on the grid that starts and returns at the tile labeled S."
  [grid]
  (let [start (first (mfind grid #(= \S %)))
        cur (first (connected-tiles grid start))]
    (loop [[cur, :as path] (list cur start)]
      (if (= cur start)
        path
        (recur (cons (next-tile grid path) path))))))


(defn part-1 [grid] (long (/ (count (find-loop grid)) 2)))


(defn- straight? [grid pos]
  (let [shape (mget grid pos)]
    (or (#{\| \-} shape)
        ;; Ugly but necessary because S can be any shape:
        (and (= \S shape)
             (or (and (connect? grid pos (v+ pos north)) (connect? grid pos (v+ pos south)))
                 (and (connect? grid pos (v+ pos west)) (connect? grid pos (v+ pos east))))))))


(defn- analyze-loop [grid path]
  (loop [path path
         corners (transient #{})
         straight-segments (transient #{})]
    (if-let [p (first path)]
      (if (straight? grid p)
        (recur (rest path) corners (conj! straight-segments p))
        (recur (rest path) (conj! corners p) straight-segments))
      {:corners (count corners)
       :straight-segments (count straight-segments)})))


(defn- part-2
  "The loop is an irregular polygon. Compute the area and subtract fractions
  of tiles to accomodate for the fact that a pipe occupies one 'unit' of area."
  [grid]
  (let [path (find-loop grid)
        {:keys [corners straight-segments]} (analyze-loop grid path)]
    (- (shoelace path)
       (/ (- corners 2) 2)
       (/ straight-segments 2))))


(let [grid (char-matrix (str/split-lines (get-puzzle-input 23 10)))]
  (println "Part 1: " (time (part-1 grid)))
  (println "Part 2: " (time (part-2 grid))))
