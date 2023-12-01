(ns aoc-23.util
  (:require
   [clojure.java.io :as io]))


(defn get-input
  ([n] (get-input n false))
  ([n test?]
   (-> (str "input/" (format "%02d" n) (when test? ".test") ".txt")
       io/resource
       slurp)))
