(ns aoc.23.21
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix inside? mfind mget mshape v+]]
   [clojure.string :as str]))


(def garden (char-matrix ["..........."
                          ".....###.#."
                          ".###.##..#."
                          "..#.#...#.."
                          "....#.#...."
                          ".##..S####."
                          ".##..#...#."
                          ".......##.."
                          ".##.#.####."
                          ".##..##.##."
                          "..........."]))


(mfind garden #(= \S %))
(mshape garden)


(def ^:private north [-1 0])
(def ^:private west [0 -1])
(def ^:private south [1 0])
(def ^:private east [0 1])


(def ^:private reachable
  (memoize
   (fn [garden v]
     (->> (map #(v+ v %) [north west south east])
          (filter #(and (inside? garden %)
                        (not= \# (mget garden %))))))))



(def step-arguments (atom #{}))

(defn- step [garden vs] 
  (swap! step-arguments #(conj % vs))
  (into #{} (mapcat (partial reachable garden) vs)))


(defn part-1 [garden]
  (let [start (first (mfind garden #(= \S %)))]
    (count (first (drop 1000 (iterate #(step garden %) [start])))))
  (println "distinct args:" (count @step-arguments)))


(let [garden (char-matrix (str/split-lines (get-puzzle-input 23 21)))]
  (println "Part 1:" (time (part-1 garden))))
