(ns aoc.24.11
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]))


(defn- half-numbers [n]
  (let [nstr (str n)
        l (count nstr)]
    (assert (even? l) "cannot halve number with uneven number of digits")
    (map parse-long [(subs nstr 0 (/ l 2)) (subs nstr (/ l 2))])))


(def blink
  "Given a stone with engraving `x`, how many stones would you have after blinking at it `n` times?"
  (memoize (fn [n x]
             (cond
               (= 0 n) 1
               (= 0 x) (blink (dec n) 1)
               (even? (count (str x))) (let [[x1 x2] (half-numbers x), n' (dec n)]
                                         (+ (blink n' x1) (blink n' x2))),
               :else (blink (dec n) (* 2024 x))))))


(defn part-1 [nums] (->> nums (map #(blink 25 %)) (reduce +)))
(defn part-2 [nums] (->> nums (map #(blink 75 %)) (reduce +)))


(let [nums (parse-longs (get-puzzle-input 24 11))]
  (println "Part 1:" (time (part-1 nums)))
  (println "Part 2:" (time (part-2 nums))))
