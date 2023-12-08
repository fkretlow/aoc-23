(ns aoc-23.08
  (:require
   [aoc-23.util :refer [get-puzzle-input lcm map-first]]
   [clojure.string :as str]))


(defn- ->graph [nodes]
  (into {}
        (for [[node left right] (partition 3 (re-seq #"[A-Z]{3}" nodes))]
          [node [left right]])))


(defn- walk
  "Assuming we have walked `n` edges before, walk from the vertex `v` to the next
  vertex using the instruction at index `i`. Return the next vertex, the next 
  instruction index, and the count of all steps taken so far."
  [graph instructions [v i n]]
  (let [[l r] (graph v)]
    [(case (nth instructions i) \L l, \R r)
     (rem (inc i) (count instructions))
     (inc n)]))


(defn- next-on-path
  "Use `walk` to iterate over the path starting at vertex `v` with the instruction at
  index `i`, assuming we have taken `n` steps before. Stop and return when the current
  vertex satisfies `pred`."
  [graph instructions pred [v i n]]
  (->> (next (iterate (partial walk graph instructions) [v i n]))
       (drop-while #(not (pred (first %))))
       (first)))


(defn part-1 [graph instructions]
  (-> (next-on-path graph instructions #(= "ZZZ" %) ["AAA" 0 0]) last))


(defn part-2
  "Expensive solution that is 'correct' in the sense that it returns the correct result 
  for any input that adheres only to the problem description:
  For any starting vertex, construct a lazy sequence of successive ending vertices 
  and the sum of the steps taken to reach it. Walk these in parallel, always consuming 
  the next head with the lowest number of steps. Stop at the first position where 
  the number of steps is equal for all the sequences."
  [graph instructions]
  (letfn [(-is-start? [v] (str/ends-with? v "A"))
          (-is-end? [v] (str/ends-with? v "Z"))
          (-next-end [[v i n]] (next-on-path graph instructions -is-end? [v i n]))]
    (loop [current-vertices (for [v (filter -is-start? (keys graph))] (-next-end [v 0 0]))]
      (if (apply = (map last current-vertices))
        (last (first current-vertices))
        (recur (sort-by last (map-first -next-end current-vertices)))))))


(defn part-2-cheap
  "Cheap, 'guessable' solution that is only correct for inputs with additional 
  constraints, presumably imposed by the process of input generation:
  Find the path to the first potential ending vertex from all the start vertices 
  and calculate the least common multiple of the lengths of those paths."
  [graph instructions]
  (apply lcm (for [v (filter #(str/ends-with? % "A") (keys graph))]
               (last (next-on-path graph instructions #(str/ends-with? % "Z") [v 0 0])))))


(let [[instructions vertices] (str/split (get-puzzle-input 8) #"\n\n")
      graph (->graph vertices)]
  (println "Part 1: " (part-1 graph instructions))
  (println "Part 2: " (part-2-cheap graph instructions)))
