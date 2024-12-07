(ns aoc.24.07
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]))

(defn- parse [input]
  (->> input
       (str/split-lines)
       (map parse-longs)))

(defn- has-solution? [fns [result & operands]]
  ((fn dfs [acc operands]
     (cond
       (< result acc) false
       (empty? operands) (= result acc)
       :else (some true? (for [f fns] (dfs (f acc (first operands)) (rest operands))))))
   (first operands) (rest operands)))

(defn part-1 [problems]
  (->> problems
       (filter (partial has-solution? [+ *]))
       (map first)
       (reduce +)))

(defn- || [x y] (parse-long (str x y)))

(defn part-2 [problems]
  (->> problems
       (filter (partial has-solution? [+ * ||]))
       (map first)
       (reduce +)))

(let [problems (parse (get-puzzle-input 24 7))]
  (println "Part 1:" (time (part-1 problems)))
  (println "Part 2:" (time (part-2 problems))))
