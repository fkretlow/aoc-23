(ns aoc.24.05
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.math :refer [floor]]
   [clojure.set :as set]
   [clojure.string :as str]))


; For our implementation, a "rule" is a pair of a page number p and a set of page numbers p_i.
; The meaning of such a rule is that each of the p_i can only ever appear *before* p.
; An "update" is an ordered sequence of page numbers that may or may not violate some rules.

(defn- parse-rules [rules-str]
  (->> rules-str
       (str/split-lines)
       (map parse-longs)
       (reduce (fn [graph [before after]] (update graph after #(conj (or % #{}) before))) {})))

(defn- parse-updates [updates-str]
  (->> updates-str
       (str/split-lines)
       (map parse-longs)
       (map vec)))

(defn- parse-input [input]
  (let [[rules-str updates-str] (str/split input #"\n\n")]
    {:rules (parse-rules rules-str)
     :updates (parse-updates updates-str)}))

(defn- in-correct-order?
  "Given a map of rules and a sequence of pages, determine whether the ordering
  of the pages conforms to the rules."
  [rules pages]
  (loop [[cur & more] pages
         forbidden? #{}]
    (or (not cur)
        (and (not (forbidden? cur))
             (recur more (set/union forbidden? (get rules cur)))))))

(defn- swap [coll k1 k2] (assoc coll k1 (coll k2) k2 (coll k1)))

(defn- fix-order
  "Given a map of rules and a sequence of pages, fix the ordering of the pages
  so it conforms to the rules."
  [rules pages]
  (loop [pages pages]
    (if (in-correct-order? rules pages)
      pages
      ; Find the first pair of indices of pages that violate the rules and swap them.
      ; Rinse and repeat until all pages are in the correct order.
      (let [[i j] (first (for [i (range (count pages))
                               j (range (inc i) (count pages))
                               :let [p1 (nth pages i)
                                     p2 (nth pages j)]
                               :when (contains? (get rules p1) p2)]
                           [i j]))]
        (recur (swap pages i j))))))

(defn- get-middle-element [v] (v (int (floor (/ (count v) 2)))))

(defn part-1 [{:keys [rules updates]}]
  (->> updates
       (filter (partial in-correct-order? rules))
       (map get-middle-element)
       (reduce +)))

(defn part-2 [{:keys [rules updates]}]
  (->> updates
       (filter (complement (partial in-correct-order? rules)))
       (map (partial fix-order rules))
       (map get-middle-element)
       (reduce +)))

(let [problem (parse-input (get-puzzle-input 24 5))]
  (println "Part 1:" (time (part-1 problem)))
  (println "Part 2:" (time (part-2 problem))))
