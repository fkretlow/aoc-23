(ns aoc-23.02
  (:require
   [aoc-23.util :refer [get-puzzle-input merge-max]]
   [clojure.string :as str]))


(defn- parse-cube-set
  "`\"2 red, 17 green\"` -> `{:red 2, :green 17}`"
  [s]
  (->> (re-seq #"(\d+) (\w+)" s)
       (map next)
       (map (fn [[number color]] [(keyword color) (parse-long number)]))
       (into {})))


(defn- parse-game
  "`\"Game 1: 2 red, 5 green; 3 red, 2 blue\"`
  -> `{:id 1, :cube-sets ({:red 2, :green 5} {:red 3, :blue 2})}`"
  [s]
  (let [[game & cube-sets] (str/split s #"[:;]")]
    {:id (-> (re-find #"\d+" game) parse-long)
     :cube-sets (map parse-cube-set cube-sets)}))


(defn- possible-game?
  "Given the actual contents of the bag, determine if the given game was possible."
  [bag-contents game]
  (every?
   (fn [[color number]] (<= number (get bag-contents color 0)))
   (apply merge-max (:cube-sets game))))


(defn part-1 [games]
  (let [bag-contents {:red 12, :green 13, :blue 14}]
    (->> (filter (partial possible-game? bag-contents) games)
         (map :id)
         (apply +))))


(defn part-2 [games]
  (->> (map :cube-sets games)
       (map #(apply merge-max %))
       (map vals)
       (map #(apply * %))
       (apply +)))


(let [games (->> (get-puzzle-input 2) (str/split-lines) (map parse-game))]
  (println "Part 1: " (part-1 games))
  (println "Part 2: " (part-2 games)))
