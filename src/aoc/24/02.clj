(ns aoc.24.02
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.seq :refer [drop-nth]]
   [clojure.string :as str]))


(defn- parse-lists [input]
  (map parse-longs (str/split-lines input)))


(defn is-safe-level? [levels i]
  (let [start (max 0 (dec i))
        end (+ i 2)
        window (take (- end start) (drop start levels))
        steps (partition 2 1 window)]
    (and (->> steps
              (map (partial apply #(abs (- %2 %1))))
              (every? #(< 0 % 4)))
         (or (apply < window) (apply > window)))))


(defn is-safe-report? [can-drop? levels]
  (or (every? #(is-safe-level? levels %) (range (count levels)))
      (and can-drop?
           (some #(is-safe-report? false (drop-nth % levels)) (range (count levels))))))


(defn part-1 [reports]
  (count (filter #(is-safe-report? false %) reports)))


(defn part-2 [reports]
  (count (filter #(is-safe-report? true %) reports)))


(let [reports (parse-lists (get-puzzle-input 24 2))]
  (println "Part 1:" (part-1 reports))
  (println "Part 2:" (part-2 reports)))
