(ns aoc.24.12
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mget mshape v+]]
   [aoc.lib.queue :refer [queue]]
   [aoc.lib.seq :refer [join-connected]]
   [clojure.string :as str]))


(def ^:private dirs {:n [-1 0], :e [0 1], :s [1 0], :w [0 -1]})


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
                  region (or region {:plant plant, :plots [] , :fences []})
                  {:keys [same-region-plots other-region-plots off-grid-plots]}
                  (->> (map (fn [[k d]] [k (v+ d pos)]) dirs)
                       (group-by (fn [[_k pos']] (cond (off-grid? pos') :off-grid-plots,
                                                       (= plant (mget grid pos')) :same-region-plots,
                                                       :else :other-region-plots))))
                  fences (concat other-region-plots off-grid-plots)]
              (recur regions
                     (-> region (update :plots #(conj % pos)) (update :fences #(apply conj % fences)))
                     (reduce conj region-queue' (map second same-region-plots))
                     (reduce conj global-queue (map second other-region-plots))
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
       (map (fn [{:keys [plots fences]}] (* (count plots) (count fences))))
       (reduce +)))


(defn- fences->sides
  "Given all fence elements as a collection of pairs `[dir pos]`, count the number of fence sides."
  [fences]
  (->> fences
       ; Group by direction and row or col respectively.
       (map (fn [[d [i j]]] (cond (#{:n :s} d) [[d i] j], (#{:e :w} d) [[d j] i])))
       (group-by first)
       (vals)
       ; Keep only the other, "secondary" indices in ascending order.
       (map (partial map second))
       (map sort)
       ; Count the number of sub-segments with consecutive distances = 1.
       (mapcat (partial join-connected #(= 1 (- %2 %1))))
       (count)))


(defn part-2 [grid]
  (->> (find-regions grid)
       (map (fn [{:keys [plots fences]}] (* (count plots) (fences->sides fences))))
       (reduce +)))


(let [grid (char-matrix (str/split-lines (get-puzzle-input 24 12)))]
  (println "Part 1:" (time (part-1 grid)))
  (println "Part 2:" (time (part-2 grid))))
