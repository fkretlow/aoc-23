(ns aoc.24.16
  (:require
   [aoc.lib.matrix :refer [char-matrix mfind mget v+]]
   [clojure.string :as str]))


(def ^:private test-input
  "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")


(def ^:private north [-1 0])
(def ^:private east [0 1])
(def ^:private south [1 0])
(def ^:private west [0 -1])
(def ^:private dirs {:n north, :e east, :s south, :w west})

(defn- turn-left [dir] (case dir :n :w, :e :n, :s :e, :w :s, (throw (ex-info "funny direction" {:direction dir}))))
(defn- turn-right [dir] (case dir :n :e, :e :s, :s :w, :w :n, (throw (ex-info "funny direction" {:direction dir}))))

(defn- parse [input]
  (let [grid (char-matrix (str/split-lines input))
        end (first (mfind grid #(= \E %)))
        pos (first (mfind grid #(= \S %)))
        dir :e]
    {:grid grid, :end end, :pos pos, :dir dir}))


(defn- next-moves [grid pos dir]
  (let [pos' (v+ pos (dirs dir))
        wall? (= (mget grid pos') \#)]
    (cond->> [[pos (turn-left dir) 1000] [pos (turn-right dir) 1000]]
      (not wall?) (cons [pos' dir 1]))))


(def ^:private dfs
  (memoize
   (fn [grid end pos dir]
     (println pos dir)
     (if (= pos end)
       0
       (apply min (for [[pos' dir' cost] (next-moves grid pos dir)]
                    (+ cost (dfs grid end pos' dir'))))))))


(defn part-1 [{:keys [grid end pos dir]}]
  (dfs grid end pos dir))


(part-1 (parse test-input))
