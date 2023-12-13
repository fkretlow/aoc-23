(ns aoc.23.12
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]))


(def reports ["???.### 1,1,3"
              ".??..??...?##. 1,1,3"
              "?#?#?#?#?#?#?#? 1,3,1,6"
              "????.#...#... 4,1,1"
              "????.######..#####. 1,6,5"
              "?###???????? 3,2,1"])


(defn can-place?-and-advance
  "Given the remaining string s and the length of the next group, determine
  whether you can place the group at index 0. Also return the number of chars
  to advance regardless of whether you do place the group."
  [s len]
  (let [last-conflict (str/last-index-of (subs s 0 len) ".")]
    (cond
      last-conflict [false (inc last-conflict)]
      (and (> (count s) len) (= \# (nth s len))) [false 1]
      :else [true (inc len)])))

; (assert (= [true 4] (can-place?-and-advance "#???" 3)))
; (assert (= [false 1] (can-place?-and-advance "#??#" 3)))
; (assert (= [true 4] (can-place?-and-advance "#??." 3)))
; (assert (= [false 3] (can-place?-and-advance "??.#" 3)))
; (assert (= [false 3] (can-place?-and-advance "??.." 3)))
; (assert (= [false 3] (can-place?-and-advance "??.#" 3)))
; (assert (= [false 1] (can-place?-and-advance "???#" 3)))
; (assert (= [false 3] (can-place?-and-advance "#?.#" 3)))


(defn choices
  "Given the remaining string s and the length of the next group, determine
  your choices for the current position: Place the group (true) and/or don't
  place it (false). Each choice also contains the number of chars to advance."
  [s len]
  (when (<= len (count s))
    (let [must-place? (= \# (first s))
          [can-place? advance] (can-place?-and-advance s len)]
      (filter some? [(when (or can-place? (not must-place?)) [can-place? advance])
                     (when (and can-place? (not must-place?)) [false 1])]))))


(defn count-arrangements
  "Given a string and the remaining groups, determine the number of possible
  arrangements in s."
  [s lens]
  (cond
    (and s (seq lens))
    (apply + (for [[place? advance] (choices s (first lens))]
               (count-arrangements
                (when (< advance (count s)) (subs s advance))
                (if place? (rest lens) lens))))

    (and s (not (seq lens)))
    (if (not (some #(= \# %) s)) 1 0)

    (and (not s) (seq lens))
    0

    (and (not s) (not (seq lens)))
    1))


(defn part-1 [lines]
  (->> (for [line lines]
         (let [[s lens-str] (str/split line #"\s")
               lens (parse-longs lens-str)]
           (count-arrangements s lens)))
       (apply +)))


(let [lines (str/split-lines (get-puzzle-input 23 12))]
  (println "Part 1: " (part-1 lines)))
