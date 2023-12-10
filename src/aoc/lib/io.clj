(ns aoc.lib.io
  (:require
   [clojure.java.io :as io]))


(def get-puzzle-input
  (memoize
   (fn [year day]
     (-> (str "input/" year "/" (format "%02d" day) ".txt")
         io/resource
         slurp))))
