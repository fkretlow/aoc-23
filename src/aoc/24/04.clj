(ns aoc.24.04
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mfind mget mshape]]
   [clojure.string :as str]))


(defn- north [[i j]] [(dec i) j])
(defn- north-east [[i j]] [(dec i) (inc j)])
(defn- east [[i j]] [i (inc j)])
(defn- south-east [[i j]] [(inc i) (inc j)])
(defn- south [[i j]] [(inc i) j])
(defn- south-west [[i j]] [(inc i) (dec j)])
(defn- west [[i j]] [i (dec j)])
(defn- north-west [[i j]] [(dec i) (dec j)])

(defn- out-of-bounds? [m [i j]] (let [[w h] (mshape m)] (not (and (<= 0 j (dec w)) (<= 0 i (dec h))))))
(defn- valid-line? [m positions] (not (out-of-bounds? m (last positions))))

(defn project-lines [m length start]
  (->> [north north-east east south-east south south-west west north-west]
       (map #(take length (iterate % start)))
       (filter #(valid-line? m %))))

(defn- count-matches-from [m word start]
  (let [lines (project-lines m (count word) start)]
    (->> (for [line lines
               :let [s (apply str (map #(mget m %) line))]]
           (if (= s word) 1 0))
         (reduce +))))

(defn part-1 [m]
  (let [xpos (mfind m #(= \X %))]
    (->> xpos
         (map #(count-matches-from m "XMAS" %))
         (reduce +))))


(defn part-2 [m]
  (->> (mfind m #(= \A %))
       (filter (let [[w h] (mshape m)]
                 (fn is-inside? [[i j]] (and (< 0 i (dec h)) (< 0 j (dec w))))))
       (filter (fn is-criss-cross? [p]
                 (let [s1 (str (mget m (north-west p)) \A (mget m (south-east p)))
                       s2 (str (mget m (north-east p)) \A (mget m (south-west p)))]
                   (and (or (= "MAS" s1) (= "SAM" s1))
                        (or (= "MAS" s2) (= "SAM" s2))))))
       (count)))

(let [m (char-matrix (str/split-lines (get-puzzle-input 24 4)))]
  (println "Part 1:" (time (part-1 m)))
  (println "Part 2:" (time (part-2 m))))

