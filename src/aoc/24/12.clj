(ns aoc.24.12
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mget mshape]]
   [aoc.lib.queue :refer [queue]]
   [clojure.string :as str]))


(def ^:private test-input
  "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")


(def ^:private dirs [(fn north [[i j]] [(dec i) j])
                     (fn east [[i j]] [i (inc j)])
                     (fn south [[i j]] [(inc i) j])
                     (fn west [[i j]] [i (dec j)])])


(defn find-regions
  "Given a char matrix representing the garden map, find all regions containing the same plants.
  Return a vector of maps with the keys `:plant`, `:plots`, and `:fences`.
  This is a breadth-first-search with two separate queues for the vertices to process.
  We process all connected vertices with the same plant first, thus exploring whole regions
  at once. Plots in other regions are added to the second queue and processed in the same manner
  afterwards."
  [grid]
  (let [off-grid? (let [[h w] (mshape grid)] (fn [[i j]] (not (and (< -1 i h) (< -1 j w)))))]
    (loop [regions []
           region nil
           region-queue (queue [[0 0]])
           global-queue (queue)
           visited? #{}]
      (cond
        ; We still have plots for the current region to process.
        (seq region-queue)
        (let [pos (peek region-queue)
              region-queue' (pop region-queue)]
          (if (visited? pos)
            (recur regions region region-queue' global-queue visited?)
            (let [plant (mget grid pos)
                  region (or region {:plant plant, :plots 0, :fences 0})
                  {:keys [same-region-plots other-region-plots]}
                  (->> (map #(% pos) dirs)
                       (group-by (fn [pos'] (cond (off-grid? pos') :off-grid-plots,
                                                  (= plant (mget grid pos')) :same-region-plots,
                                                  :else :other-region-plots))))
                  fences (- 4 (count same-region-plots))]
              (recur regions
                     (-> region (update :plots inc) (update :fences #(+ fences %)))
                     (reduce conj region-queue' same-region-plots)
                     (reduce conj global-queue other-region-plots)
                     (conj visited? pos)))))

        ; Region finished. Take the next plot from the global queue and start processing its region.
        (seq global-queue)
        (let [pos (peek global-queue)
              global-queue' (pop global-queue)]
          (if (visited? pos)
            (recur regions region region-queue global-queue' visited?)
            (recur (conj regions region) nil (conj region-queue pos) global-queue' visited?)))

        ; All plots have been processed. Return the regions we have collected.
        :else (cond-> regions region (conj region))))))


(defn part-1 [grid]
  (->> (find-regions grid)
       (map (fn [{:keys [plots fences]}] (* plots fences)))
       (reduce +)))


(let [grid (char-matrix (str/split-lines (get-puzzle-input 24 12)))]
  (println "Part 1:" (time (part-1 grid))))
