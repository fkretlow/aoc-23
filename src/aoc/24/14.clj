(ns aoc.24.14
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.seq :refer [find-first]]
   [clojure.string :as str]))


(def ^:private width 101)
(def ^:private height 103)
(def ^:private quadrants [[[0 0] [50 51]], [[51 0] [101 51]], [[0 52] [50 103]], [[51 52] [101 103]]])


(defn- parse [input]
  (->> (str/split-lines input)
       (map parse-longs)
       (map #(partition 2 %))))


(defn- move
  "Given a robot's position at `(x,y)` and velocity `(dx,dy)`, calculate its next position."
  [[x y] [dx dy]]
  [(mod (+ x dx) width) (mod (+ y dy) height)])


(defn- step
  "Given all robots as a pair of current position and velocity, move all of them and return
  their new state in the same form."
  [robots]
  (for [[p d] robots] [(move p d) d]))


(defn- ->quadrant
  "Given a point `(x,y)`, determine the index of the quadrant it is in. Returns nil if no quadrant matches."
  [[x y]]
  (->> quadrants
       (map-indexed
        (fn [i [[xmin ymin] [xmax ymax]]]
          (when (and (< (dec xmin) x xmax) (< (dec ymin) y ymax)) i)))
       (find-first some?)))


(defn- part-1 [robots]
  (let [robots' (nth (iterate step robots) 100)]
    (->> robots'
         (map (fn [[p]] (when-let [q (->quadrant p)] [q p])))
         (filter some?)
         (group-by first)
         (vals)
         (map count)
         (reduce *))))


(let [robots (parse (get-puzzle-input 24 14))]
  (println "Part 1:" (time (part-1 robots))))
