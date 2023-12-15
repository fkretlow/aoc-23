(ns aoc.23.15
  (:refer-clojure :exclude [hash])
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.string :as str]))


(defn- hash [s]
  (loop [acc (int 0)
         i (int 0)]
    (if (= (count s) i)
      acc
      (recur (rem (* (+ acc (int (nth s i))) 17) 256)
             (inc i)))))


(defn part-1 [instruction-strs]
  (transduce (map hash) + instruction-strs))


;; For part 2, boxes are linked lists where the head of the list is the first lens.
;; Lenses are vectors of two elements, the label and the focal length.


(defn- remove-lens [box [label]]
  (list* (remove #(= label (first %)) box)))


(defn- set-lens [box [label focal-length]]
  (if-let [lens (first box)]
    (if (= label (first lens))
      (cons [label focal-length] (rest box))
      (cons lens (set-lens (rest box) [label focal-length])))
    (cons [label focal-length] nil)))


(defn- parse-instruction [instruction]
  (let [[_ label op n] (re-find #"^(\w+)([=-])(\d+)?$" instruction)]
    {:op (case op "=" :set, "-" :remove),
     :lens [label (when n (parse-long n))]}))


(defn- execute [instructions]
  (->> (reduce
        (fn [boxes {:keys [op lens]}]
          (let [i (hash (first lens)), box (nth boxes i)]
            (case op
              :set (assoc! boxes i (set-lens box lens))
              :remove (assoc! boxes i (remove-lens box lens)))))
        (transient (vec (repeat 256 nil)))
        instructions)
       (persistent!)
       (mapv vec)))


(defn- total-focusing-power [boxes]
  (->> (for [i (range (count boxes))
             j (range (count (nth boxes i)))
             :let [[_ v] (get-in boxes [i j])]]
         (* (inc i) (inc j) v))
       (apply +)))


(defn part-2 [instruction-strs]
  (-> (map parse-instruction instruction-strs)
      (execute)
      (total-focusing-power)))


(let [instruction-strs (str/split (str/trim (get-puzzle-input 23 15)) #",")]
  (println "Part 1: " (part-1 instruction-strs))
  (println "Part 2: " (part-2 instruction-strs)))
