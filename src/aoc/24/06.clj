(ns aoc.24.06
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mfind mget mshape v+]]
   [clojure.string :as str]))

(def ^:private dir? #{:n :e :s :w})

(defn- turn-right [dir]
  {:pre [(dir? dir)], :post [dir?]}
  (case dir :n :e, :e :s, :s :w, :w :n))

(defn- step [pos dir]
  {:pre [(dir? dir)], :post [dir?]}
  (v+ pos (case dir :n [-1 0], :e [0 1], :s [1 0], :w [0 -1])))

(defn- out-of-bounds? [[h w] [i j]]
  {:pre [(every? int? [h w i j])]}
  (or (< i 0) (< j 0) (>= i h) (>= j w)))

(defn- ->situation [m pos dir]
  {:post [#{:edge :obs :clear}]}
  (let [pos' (step pos dir)]
    (cond
      (out-of-bounds? (mshape m) pos') :edge,
      (= \# (mget m pos')) :obs,
      :else :clear)))

(defmulti advance
  (fn [m {:keys [pos dir]}] (->situation m pos dir)))

(defmethod advance :clear [_ {:keys [pos dir], :as state}]
  (let [pos' (step pos dir)]
    (-> state
        (assoc :pos pos')
        (update :path #(conj % pos')))))

(defmethod advance :obs [_ {:keys [pos dir], :as state}]
  (-> state
      (update-in [:obs-hit-from (step pos dir)] #(conj (or % #{}) dir))
      (update :dir turn-right)))

(defmethod advance :edge [_ state]
  (assoc state :finished? true))

(defn part-1 [m state]
  (let [advance (partial advance m)]
   (loop [state state]
    (if (:finished? state) (-> state :path set count) (recur (advance state))))))

(defn- parse [input]
  (let [m (char-matrix (str/split-lines input))
        pos (first (mfind m #(= \^ %)))]
    [m {:pos pos, :dir :n, :path [pos], :obs-hit-from {}}]))

(def ^:private test-input
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(let [[m state] (parse (get-puzzle-input 24 6))]
  (println "Part 1:" (time (part-1 m state))))
