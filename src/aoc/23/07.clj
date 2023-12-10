(ns aoc.23.07
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.seq :refer [map-first pad-end]]
   [clojure.string :as str]))


(def ^:private card-ranks
  (into {} (map-indexed #(-> [%2 (char (+ (int \a) %1))]) "*23456789TJQKA")))


(defn- ->type
  "Given a string representing a poker hand, return the
  counts of each distinct card in the hand in descending
  order as an integer of five digits (padded with 0s).
  Jokers (\\*) are extracted first and added to the highest
  count. This representation of the hand type is sortable.

  Examples:
  `\"KTJJT\"` -> `22100`
  `\"KT**T\"` -> `41000`"
  [hand]
  (let [jokers (count (re-seq #"\*" hand))]
    (->> (remove #(= \* %) hand)
         (frequencies)
         (vals)
         (sort >)
         (map-first #(+ jokers %))
         (pad-end 5 0)
         (reduce (fn [acc x] (+ (* 10 acc) x))))))


(defn- ->comparable
  "Make a poker hand comparable by mapping it to a pair of a
  comparable representation of its type (see `->type`) and a
  string where the successive char values represent card values.

  Examples:
  `\"KTJJT\"` -> `[22100 \"mjkkj\"]`
  `\"KT**T\"` -> `[41000 \"mjaaj\"]`"
  [hand]
  [(->type hand)
   (apply str (map card-ranks hand))])


(defn- calculate-winnings [games]
  (->> (for [game games] (map-first ->comparable game))
       (sort-by first)
       (map second)
       (map-indexed (fn [i bid] (* (inc i) bid)))
       (apply +)))


(def part-1 calculate-winnings)


(defn part-2 [games]
  (calculate-winnings
   (for [game games] (map-first #(str/replace % \J \*) game))))


(let [games (->> (get-puzzle-input 23 7)
                 (str/split-lines)
                 (map #(str/split % #"\s"))
                 (map (fn [[hand bid]] [hand (parse-long bid)])))]
  (println "Part 1: " (part-1 games))
  (println "Part 2: " (part-2 games)))
