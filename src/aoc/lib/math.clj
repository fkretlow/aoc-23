(ns aoc.lib.math 
  (:require
   [clojure.math :refer [ceil]]))


(defn lcm [& xs]
  (reduce #(.divide (.multiply %1 %2) (.gcd %1 %2))
          (map biginteger xs)))


(defn next-greater-int
  "Return the closest integer strictly greater than `x`."
  [x]
  (let [n (int (ceil x))]
    (if (= (int x) n) (inc n) n)))

