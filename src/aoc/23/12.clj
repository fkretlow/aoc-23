(ns aoc.23.12
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.seq :refer [min-of-nilable]]
   [clojure.string :as str]))


(defn- ->report [line]
  (let [[s nums] (str/split line #"\s")]
    [s (parse-longs nums)]))


(defn- ->unfolded-report [line]
  (let [[s nums] (->report line)]
    [(str/join "?" (repeat 5 s)) (flatten (repeat 5 nums))]))


(defn- choices
  "Given a string s and a group length n, return a vector of three elements:
  1. whether you *must* place the group at index 0 (boolean)
  2. whether you *can* place the group at index 0 (boolean)
  3. if you can't *and* needn't place it, how many characters you can skip
     (nilable integer)"
  [s n]
  (let [must-place? (= \# (first s))
        last-dot (str/last-index-of (subs s 0 n) ".")
        first-hash (some-> (str/index-of (subs s 1 n) "#") inc)
        padding? (or (= n (count s)) (not= \# (nth s n)))
        can-place? (and (not last-dot) padding?)
        skip (when (and (not must-place?) (not can-place?))
               (min-of-nilable (some-> last-dot inc) first-hash 1))]
    [must-place? can-place? skip]))


(def count-arrangements
  (memoize
   (fn [s [n, :as nums]]
     (cond
       (empty? s) (if (not n) 1 0)
       (not n) (if (str/includes? s "#") 0 1)
       (> n (count s)) 0
       :else (let [group-length (cond-> n (< n (count s)) (inc))
                   [must-place? can-place? skip] (choices s n)]
               (case [must-place? can-place?]
                 [true true] (count-arrangements (subs s group-length) (rest nums))
                 [true false] 0
                 [false true] (+ (count-arrangements (subs s group-length) (rest nums))
                                 (count-arrangements (subs s 1) nums))
                 [false false] (count-arrangements (subs s skip) nums)))))))


(defn solve [parser lines]
  (->> (map parser lines)
       (map #(apply count-arrangements %))
       (apply +)))


(let [lines (str/split-lines (get-puzzle-input 23 12))]
  (println "Part 1:" (time (solve ->report lines)))
  (println "Part 2:" (time (solve ->unfolded-report lines))))
