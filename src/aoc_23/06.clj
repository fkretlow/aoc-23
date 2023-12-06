(ns aoc-23.06
  (:require
   [aoc-23.util :refer [get-puzzle-input next-greater-int parse-longs]]
   [clojure.math :refer [ceil sqrt]]
   [clojure.string :as str]))


;; For any given input race the following inequality holds 
;; when we beat the previous record:
;;
;;   d_rec < t_b * (t - t_b) 
;;
;; where
;;   * t is the total time for the race,
;;   * d_rec is the maximum distance reached so far, and
;;   * t_b is our variable: how long we press the button.
;;
;; Thus we can compute the succesful button press durations 
;; by solving the roots of
;;
;;   -t_b^2 + t*t_b - d_rec = 0
;;
;; which is easily done with the quadratic formula, and 
;; rounding both solutions up to the next greater integer 
;; (strictly greater for the lower bound).


(defn- calculate-winning-range [t d_rec]
  (let [discriminant (- (* t t) (* 4 d_rec))]
    (when-not (neg? discriminant)
      (let [root (sqrt discriminant)]
        [(next-greater-int (/ (+ (- t) root) (- 2)))
         (int (ceil (/ (- (- t) root) (- 2))))]))))


(defn part-1 [races]
  (->> (for [[t d_rec] races] (calculate-winning-range t d_rec))
       (filter some?)
       (map (fn [[start end]] (- end start)))
       (apply *)))


(defn part-2 [[t d_rec]]
  (let [[start end] (calculate-winning-range t d_rec)]
    (- end start)))


(let [lines (str/split-lines (get-puzzle-input 6))]
  (let [races (apply (partial map vector) (map parse-longs lines))]
    (println "Part 1: " (part-1 races)))
  (let [race (map #(parse-long (apply str (re-seq #"\d+" %))) lines)]
    (println "Part 2: " (part-2 race))))
