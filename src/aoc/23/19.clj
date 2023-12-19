(ns aoc.23.19
  (:refer-clojure :exclude [name])
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.range-set :as rs]
   [aoc.lib.seq :refer [find-first]]
   [clojure.string :as str]))


(def ^:private base-conditions (into {} (for [c "xmas"] [(str c) [[1 4001]]])))


(defn- ->part [s]
  (reduce
   (fn [m [_ category value]] (assoc m category (parse-long value)))
   {}
   (re-seq #"([xmas])=(\d+)" s)))


(defn- ->rule [s]
  (if (str/includes? s ":")
    (let [[_ category op n dest] (re-find #"([xmas])?([<>])?(\d+)?:?(\w+)" s)]
      [{category (let [n (parse-long n)] [(case op "<" [1 n], ">" [(inc n) 4001])])}
       dest])
    [{"x" [[1 4001]]} s]))


(defn- combine-rules
  "Given a seq of rules of the form [{category: range-set} destination],
  combine them in such a way that all the rules have mutually exclusive
  range sets for every category as condition."
  [rules]
  (loop [combined-rules []
         conditions base-conditions
         rules rules]
    (if (seq rules)
      (let [[match-condition destination] (first rules)]
        (recur (conj combined-rules
                     [(merge-with #(rs/intersection %1 %2) conditions match-condition)
                      destination])
               (merge-with #(rs/difference %1 %2) conditions match-condition)
               (rest rules)))
      combined-rules)))


(defn- ->workflow [s]
  (let [[_ name rules] (re-find #"(\w+)\{(.+)\}" s)]
    [name (combine-rules (map ->rule (str/split rules #",")))]))


(defn- condition-matches? [condition part]
  (every? (fn [[category value]] (rs/contains? (get condition category) value)) part))


(defn- accept? [workflows name part]
  (let [[_ destination]
        (find-first (fn [[condition]]
                      (condition-matches? condition part)) (get workflows name))]
    (case destination
      "A" true
      "R" false
      (accept? workflows destination part))))


(defn- accepted-conditions [workflows name condition]
  (case name
    "R" nil
    "A" [condition]
    (let [workflow (get workflows name)]
      (apply
       concat
       (for [[match-condition destination] workflow]
         (accepted-conditions workflows
                              destination
                              (merge-with #(rs/intersection %1 %2)
                                          condition
                                          match-condition)))))))


(defn part-1 [workflows parts]
  (->> (filter #(accept? workflows "in" %) parts)
       (mapcat vals)
       (apply +)))


(defn part-2 [workflows]
  (->> (accepted-conditions workflows "in" base-conditions)
       (map vals)
       (map #(map rs/count-elements %))
       (map #(apply * %))
       (apply +)))


(let [[workflows-str parts-str] (str/split (get-puzzle-input 23 19) #"\n\n")
      workflows (into {} (map ->workflow (str/split-lines workflows-str)))
      parts (map ->part (str/split-lines parts-str))]
  (println "Part 1:" (time (part-1 workflows parts)))
  (println "Part 2:" (time (part-2 workflows))))
