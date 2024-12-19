(ns aoc.24.16
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mfind mget v+]]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.string :as str]))

(defn- parse [input] (char-matrix (str/split-lines input)))

(def ^:private dir->vector {:n [-1 0], :e [0 1], :s [1 0], :w [0 -1]})

(defn- walk-forward [[pos dir]]
  {:pre [(#{:n :e :s :w} dir)]}
  [(v+ pos (dir->vector dir)) dir])

(defn- turn-left [[pos dir]]
  {:pre [(#{:n :e :s :w} dir)]}
  [pos (case dir :n :w, :e :n, :s :e, :w :s)])

(defn- turn-right [[pos dir]]
  {:pre [(#{:n :e :s :w} dir)]}
  [pos (case dir :n :e, :e :s, :s :w, :w :n)])

(defn- ->edges [Q pos-and-dir]
  (->> [[walk-forward 1] [turn-left 1000] [turn-right 1000]]
       (map (fn [[f cost]] [(f pos-and-dir) cost]))
       (filter (fn [[pos]] (contains? Q pos)))))

(defn- end? [grid pos] (= \E (mget grid pos)))

(defn- dijkstra [grid]
  (let [vertices (->> (mfind grid #(not= \# %))
                      (mapcat #(-> [[% :n] [% :e] [% :s] [% :w]])))
        start [(first (mfind grid #(= \S %))) :e]]
    (loop [Q (-> (reduce (fn [Q pos] (conj Q [pos Integer/MAX_VALUE])) (priority-map) vertices)
                 (assoc start 0)),
           visited? #{}]
      (let [[[pos, :as pos-and-dir] path-cost] (peek Q)]
        (cond
          (visited? pos-and-dir) (recur (pop Q) visited?),
          (end? grid pos) (get Q pos-and-dir),
          :else (recur
                 (reduce (fn [Q [pos-and-dir step-cost]]
                           (update Q pos-and-dir #(min % (+ path-cost step-cost))))
                         Q
                         (->edges Q pos-and-dir))
                 (conj visited? pos-and-dir)))))))

(def part-1 dijkstra)

(let [grid (parse (get-puzzle-input 24 16))]
  (println "Part 1:" (time (part-1 grid))))

