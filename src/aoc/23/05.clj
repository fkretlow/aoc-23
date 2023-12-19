(ns aoc.23.05
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.range :as r]
   [clojure.string :as str])
  (:import
   [java.lang Long]))


(defn- ->map-fn [map-seq]
  (fn [n]
    (loop [ranges (partition 3 map-seq)]
      (if-let [[dest-start src-start len] (first ranges)]
        (if (<= src-start n (dec (+ src-start len)))
          (+ n (- dest-start src-start))
          (recur (rest ranges)))
        n))))


(defn part-1 [seed-seq map-seqs]
  (let [map-chain-fn (apply comp (reverse (map ->map-fn map-seqs)))]
    (as-> seed-seq seeds (apply min (map map-chain-fn seeds)))))


(defn- insert-zero-shift-ranges [mapping]
  (reverse
   (let [mapping'
         (reduce
          (fn [mapping [r' d']]
            (cond-> mapping
              (< (get (ffirst mapping) 1 0) (r' 0)) (conj [[(get (ffirst mapping) 1 0) (r' 0)] 0])
              :always (conj [r' d'])))
          nil
          mapping)]
     (conj mapping' [[(get (ffirst mapping') 1 0) Long/MAX_VALUE] 0]))))


(defn- ->mapping [map-seq]
  (->> (for [[dest-start src-start len] (partition 3 map-seq)]
         [[src-start (+ src-start len)] (- dest-start src-start)])
       (sort-by first)
       (insert-zero-shift-ranges)))


(defn- apply-mapping-to-range [mapping r]
  (->> mapping
       (drop-while (fn [[r_m]] (<= (r_m 1) (r 0))))
       (take-while (fn [[r_m]] (< (r_m 0) (r 1))))
       (map (fn [[r_m d_m]] [(r/intersection r r_m) d_m]))))


(defn- apply-mappings-to-shifted-range [mappings [r d]]
  (if (seq mappings)
    (mapcat
     (partial apply-mappings-to-shifted-range (rest mappings))
     (->> (r/shift d r)
          (apply-mapping-to-range (first mappings))
          (map (fn [[r' d']] [(r/shift (- d) r') (+ d' d)]))))
    [[r d]]))


(defn- ->seed-ranges [seed-seq]
  (->> (partition 2 seed-seq)
       (map (fn [[start len]] [start (+ start len)]))))


(defn part-2 [seed-seq map-seqs]
  (let [seed-ranges (->seed-ranges seed-seq)
        mappings (map ->mapping map-seqs)]
    (->> (mapcat (partial apply-mappings-to-shifted-range mappings)
                 (map (fn [r] [r 0]) seed-ranges))
         (map (fn [[r d]] (+ (r 0) d)))
         (apply min))))


(let [input (get-puzzle-input 23 5)
      [seed-seq & map-seqs] (map parse-longs (str/split input #"\n\n"))]
  (println "Part 1: " (part-1 seed-seq map-seqs))
  (println "Part 2: " (part-2 seed-seq map-seqs)))
