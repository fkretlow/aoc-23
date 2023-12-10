(ns aoc.lib.day
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]))


(defprotocol IDay
  (solve-part-1 [day])
  (solve-part-2 [day]))


(defrecord Day [year day part-1 part-2]
  IDay
  (solve-part-1 [_] (part-1 (get-puzzle-input year day)))
  (solve-part-2 [_] (part-1 (get-puzzle-input year day))))


(defn make-day [year day part-1 part-2]
  (->Day year day part-1 part-2))
