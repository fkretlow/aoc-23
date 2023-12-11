(ns aoc.lib.matrix)


(defn transpose
  "Transpose a two-dimensional matrix."
  [matrix] (apply (partial map vector) matrix))


(defn find-row-indices
  "Find the indices of all the rows of a two-dimensional matrix that satisfy pred."
  [pred matrix]
  (->> (map-indexed #(-> [%1 %2]) matrix)
       (filter #(pred (second %)))
       (map first)))


(defn find-col-indices
  "Find the indices of all the columns of a two-dimensional matrix that satisfy pred."
  [pred matrix]
  (->> (transpose matrix)
       (find-row-indices pred)))


(defn find-indices
  "Find the indices of all the elements of a two-dimensional matrix that satisfy pred."
  [pred matrix]
  (->> (for [row matrix] (filter some? (map-indexed (fn [j x] (when (pred x) j)) row)))
       (map-indexed (fn [i js] (when (seq js) (for [j js] [i j]))))
       (filter some?)
       (apply concat)))
