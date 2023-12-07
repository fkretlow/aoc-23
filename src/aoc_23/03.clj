(ns aoc-23.03
  (:require
   [aoc-23.util :refer [window-indices get-puzzle-input re-seq-pos]]
   [clojure.string :as str]))


(defn- extract-numbers [s]
  (->> (re-seq-pos #"\d+" s)
       (map #(assoc % :number (parse-long (:group %))))
       (map #(dissoc % :group))
       (vec)))


(defn- extract-symbols [s]
  (->> (re-seq-pos #"[^\d.]" s)
       (map #(assoc % :symbol (first (:group %))))
       (map #(dissoc % :group :end))
       (vec)))


(defn- parse-line [s]
  {:line s,
   :numbers (extract-numbers s),
   :symbols (extract-symbols s)})


(defn- is-adjacent-to-symbol? [window-of-lines number]
  (some (fn [sym] (<= (dec (:start number)) (:start sym) (:end number)))
        (mapcat :symbols window-of-lines)))


(defn- find-part-numbers [lines]
  (->> (for [[i start end] (window-indices 1 (count lines)),
             :let [window (subvec lines start end),
                   numbers (:numbers (lines i))]]
         (filter (partial is-adjacent-to-symbol? window) numbers))
       (apply concat)))


(defn part-1 [lines]
  (->> (find-part-numbers lines)
       (map :number)
       (apply +)))


(defn- calculate-ratio-when-gear [window-of-lines star]
  (let [adjacent-numbers
        (filter (fn [number] (<= (dec (:start number)) (:start star) (:end number)))
                (mapcat :numbers window-of-lines))]
    (when (= 2 (count adjacent-numbers))
      (apply * (map :number adjacent-numbers)))))


(defn part-2 [lines]
  (->> (for [[i start end] (window-indices 1 (count lines)),
             :let [window (subvec lines start end)
                   stars (filter #(= \* (:symbol %)) (:symbols (lines i)))]]
         (filter some? (map (partial calculate-ratio-when-gear window) stars)))
       (apply concat)
       (apply +)))


(let [lines (->> (get-puzzle-input 3) str/split-lines (map parse-line) vec)]
  (println "Part 1: " (part-1 lines))
  (println "Part 2: " (part-2 lines)))
