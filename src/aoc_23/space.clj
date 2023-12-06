(ns aoc-23.space
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]))


(let [i (atom 0)]
  (defn- make-id! [] (swap! i inc)))


(defn- normalize-interval [[p1 p2]]
  (apply (partial map vector) (map sort (map vector p1 p2))))


(defn- interval->points [[[x1 y1] [x2 y2]]]
  (for [x (range x1 x2), y (range y1 y2)] [x y]))


(defn- interval->adjacent-points [[[x1 y1] [x2 y2]]]
  (concat
   (for [x (range (dec x1) (inc x2)),
         y [(dec y1) y2]]
     [x y])
   (for [y (range y1 y2)
         x [(dec x1) x2]]
     [x y])))


(def make-space (constantly nil))


(defn insert-interval [E interval value]
  (let [interval (normalize-interval interval)
        id (make-id!)]
    (-> E
        (update :space (fn [-map]
                         (reduce (fn [-map point] (update -map point #(conj % id)))
                                 -map
                                 (interval->points interval))))
        (update :intervals #(assoc % id {:id id, :interval interval, :value value})))))


(defn get-intervals-at [E point]
  (for [id (get (:space E) point)] (get (:intervals E) id)))


(defn get-adjacent-intervals [E interval]
  (let [adjacent-points (-> (:interval interval)
                            normalize-interval
                            interval->adjacent-points)
        ids (apply set/union (for [p adjacent-points] (get (:space E) p)))]
    (for [id ids] (get (:intervals E) id))))


(defn get-adjacent-values [m object]
  (map :value (get-adjacent-intervals m object)))


(defn get-objects [m] (-> m :intervals vals))
(defn get-values [m] (map :value (get-objects m)))


(let [m (-> (make-space)
            (insert-interval [[1 1] [2 2]] 42)
            (insert-interval [[1 1] [3 4]] :hello))]
  (get-intervals-at m [1 1]))
