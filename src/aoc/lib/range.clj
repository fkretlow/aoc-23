(ns aoc.lib.range)


(defn merge
  "Given any number of half open integer intervals of the form `[start end]`
  (end non-inclusive), reduce them to a set of non-overlapping ranges.
  E.g. `[1 3] [5 7] [2 4]` -> `([1 4] [5 7])`"
  [& rs]
  (letfn [(-overlap? [r1 r2] (let [[r1 r2] (sort [r1 r2])] (> (r1 1) (r2 0))))
          (-merge [r1 r2] [(min (r1 0) (r2 0)) (max (r1 1) (r2 1))])]
    (sort
     (reduce
      (fn [acc r]
        (if (and (seq acc) (-overlap? r (first acc)))
          (conj (rest acc) (-merge r (first acc)))
          (conj acc r)))
      nil
      (sort (map vec rs))))))


(defn intersect [r1 r2]
  (let [[r1 r2] (sort [r1 r2])]
    [(max (r1 0) (r2 0)) (min (r1 1) (r2 1))]))


(defn shift [d r]
  [(+ d (r 0)) (+ d (r 1))])
