(ns aoc.24.10
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [mfind mget mshape]]
   [clojure.set :as set]
   [clojure.string :as str]))


(defn- int-matrix [lines]
  (into-array (for [line lines] (int-array (map (comp parse-long str) (seq (str/trim line)))))))


(defn- parse [input]
  (int-matrix (str/split-lines input)))


(def ^:private dirs [(fn north [[i j]] [(dec i) j])
                     (fn east [[i j]] [i (inc j)])
                     (fn south [[i j]] [(inc i) j])
                     (fn west [[i j]] [i (dec j)])])


(defn part-1 [grid]
  (let [on-the-grid? (let [[h w] (mshape grid)] (fn [[i j]] (and (< -1 i h) (< -1 j w))))
        dfs (fn -dfs [pos]
              (let [x (mget grid pos)]
                (if (= 9 x)
                  #{pos}
                  (->> (map #(% pos) dirs)
                       (filter on-the-grid?)
                       (filter #(= (inc x) (mget grid %)))
                       (map -dfs)
                       (reduce set/union)))))]
    (->> (mfind grid zero?)
         (map dfs)
         (map count)
         (reduce +))))


(defn part-2 [grid]
  (let [on-the-grid? (let [[h w] (mshape grid)] (fn [[i j]] (and (< -1 i h) (< -1 j w))))
        dfs (fn -dfs [pos]
              (let [x (mget grid pos)]
                (if (= 9 x)
                  1
                  (->> (map #(% pos) dirs)
                       (filter on-the-grid?)
                       (filter #(= (inc x) (mget grid %)))
                       (map -dfs)
                       (reduce +)))))]
    (->> (mfind grid zero?)
         (map dfs)
         (reduce +))))


(let [grid (parse (get-puzzle-input 24 10))]
  (println "Part 1:" (time (part-1 grid)))
  (println "Part 2:" (time (part-2 grid))))
