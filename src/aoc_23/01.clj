(ns aoc-23.01
  (:require
   [clojure.string :as str]))


(def digits (set "0123456789"))
(def number-words {"zero" 0,
                   "one" 1,
                   "two" 2,
                   "three" 3,
                   "four" 4,
                   "five" 5,
                   "six" 6,
                   "seven" 7,
                   "eight" 8,
                   "nine" 9})


(defn find-digit-numbers [line]
  (let [only-digits (filter digits line)]
    (map parse-long [(first only-digits) (last only-digits)])))


(let [[find-first-number find-last-number]
      (->> (str/join "|" (concat digits (keys number-words)))
           (#(-> [(format "^.*?(%s).*$" %) (format "^.*(%s).*$" %)]))
           (map (fn [pattern]
                  (fn [line]
                    (let [match (second (re-find (re-pattern pattern) line))]
                      (or (number-words match) (parse-long match)))))))]
  (defn find-digit-or-word-numbers [line]
    [(find-first-number line) (find-last-number line)]))


(defn sum-up-calibration-values
  "Given a function `find-numbers` that returns a seq of the first and the last
  \"number\" in a line, calculate the sum of all the calibration values for the
  given `lines`."
  [find-numbers lines]
  (->> lines
       (map find-numbers)
       (map (fn [[x y]] (+ (* 10 x) y)))
       (apply +)))


(def part-1 (partial sum-up-calibration-values find-digit-numbers))
(def part-2 (partial sum-up-calibration-values find-digit-or-word-numbers))
