(ns aoc.24.03
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]))


(defn part-1 [input]
  (let [pattern #"mul\(\d{1,3},\d{1,3}\)"]
    (->> (re-seq pattern input)
         (map parse-longs)
         (map #(apply * %))
         (reduce +))))


(defn part-2 [input]
  (let [pattern #"(?:mul\(\d{1,3},\d{1,3}\))|(?:do\(\))|(?:don't\(\))"
        instructions (re-seq pattern input)]
    (loop [acc 0
           skip? false
           [instr & more] instructions]
      (cond
        (not instr)
        acc

        skip?
        (recur acc (not= instr "do()") more)

        (str/starts-with? instr "mul")
        (let [prod (->> (parse-longs instr) (apply *))]
          (recur (+ acc prod) false more))
        
        :else
        (recur acc (= instr "don't()") more)))))


(let [input (get-puzzle-input 24 3)]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
