(ns aoc.23.13
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [transpose]]
   [aoc.lib.seq :refer [all-pairs find-first one?]]
   [clojure.string :as str]))


(defn- rotate-pattern [pattern] (mapv #(apply str %) (transpose pattern)))


(defn- is-axis?
  "Given a pattern, the index of the first line after a potential symmetry
  axis, and possibly a function of an integer that returns true if its 
  argument should be ignored during the tests, return whether this is indeed 
  the position of a symmetry axis."
  ([pattern j] (is-axis? pattern j (constantly false)))
  ([pattern j ignore?]
   (loop [i (dec j), j j]
     (cond
       (some ignore? [i j]) (recur (dec i) (inc j))
       (or (< i 0) (<= (count pattern) j)) true
       (= (nth pattern i) (nth pattern j)) (recur (dec i) (inc j))
       :else false))))


(defn- symmetry-index
  "Find the first index of a symmetry axis in the given pattern. Return nil
  when none is found. With the flag :smudge (or true) assume there's one and 
  only one smudge in the pattern."
  ([pattern] (symmetry-index false pattern))
  ([smudge? pattern]
   (if smudge?
     (->> (all-pairs (range (count pattern)))
          (filter (fn [[i j]] (one? #(apply not= %) (map vector (nth pattern i) (nth pattern j)))))
          (filter (fn [[i j]] (odd? (- i j))))
          (filter (fn [[i j]] (is-axis? pattern (/ (inc (+ i j)) 2) #{i j})))
          (map (fn [[i j]] (/ (inc (+ i j)) 2)))
          (first))
     (find-first (partial is-axis? pattern) (range 1 (count pattern))))))


(defn- symmetry
  "Find the orientation and the position of a symmetry axis in the given pattern
  if one exists. Return nil when none is found."
  ([pattern] (symmetry false pattern))
  ([smudge? pattern]
   (if-let [i (symmetry-index smudge? pattern)]
     [:horizontal i]
     (when-let [j (symmetry-index smudge? (rotate-pattern pattern))]
       [:vertical j]))))


(defn part-1 [patterns]
  (->> (map symmetry patterns)
       (map (fn [[dir i]] (case dir :horizontal (* 100 i), :vertical i)))
       (apply +)))


(defn part-2 [patterns]
  (->> (map (partial symmetry :smudge) patterns)
       (map (fn [[dir i]] (case dir :horizontal (* 100 i), :vertical i)))
       (apply +)))


(let [patterns (map str/split-lines (str/split (get-puzzle-input 23 13) #"\n\n"))]
  (println "Part 1: " (part-1 patterns))
  (println "Part 2: " (part-2 patterns)))
