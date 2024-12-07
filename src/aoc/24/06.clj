(ns aoc.24.06
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mfind mget mshape v+]]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

(s/def ::pos (s/tuple int? int?))
(s/def ::dir #{:n :e :s :w})
(s/def ::path (s/coll-of ::pos))
(s/def ::finished? boolean?)
(s/def ::obs-hit-from (s/map-of ::pos (s/and set? (s/coll-of ::dir))))
(s/def ::loop-obs (s/and set? (s/coll-of ::pos)))
(s/def ::state (s/keys :req-un [::pos ::dir ::path], :opt-un [::finished? ::obs-hit-from ::loop-obs]))
(s/def ::situation #{:edge :obs :clear})

(defn- turn-right [dir]
  {:pre [(s/valid? ::dir dir)],
   :post [(s/valid? ::dir %)]}
  (case dir :n :e, :e :s, :s :w, :w :n))

(defn- step [pos dir]
  {:pre [(s/valid? ::pos pos) (s/valid? ::dir dir)],
   :post [(s/valid? ::pos %)]}
  (v+ pos (case dir :n [-1 0], :e [0 1], :s [1 0], :w [0 -1])))

(defn- out-of-bounds? [[h w] [i j, :as pos]]
  {:pre [(s/valid? ::pos pos) (and (<= 0 h) (<= 0 w))],
   :post [boolean?]}
  (or (< i 0) (< j 0) (>= i h) (>= j w)))

(defn- ->situation [m pos dir]
  {:post [(s/valid? ::situation %)]}
  (let [pos' (step pos dir)]
    (cond
      (out-of-bounds? (mshape m) pos') :edge,
      (= \# (mget m pos')) :obs,
      :else :clear)))

(defmulti advance
  (fn [m {:keys [pos dir], :as state}]
    {:pre [(s/valid? ::state state)]}
    (->situation m pos dir)))

(defn- will-loop-if-turning-right?
  "Given the current position and direction, and the previously hit obstacles, determine
  whether we would hit any of the latter from the same direction if we turned right."
  [[i j, :as pos] dir obs-hit-from]
  {:pre [(s/valid? ::pos pos) (s/valid? ::dir dir) (s/valid? ::obs-hit-from obs-hit-from)],
   :post [boolean?]}
  (let [dir' (turn-right dir)]
    (some
     (fn [[[i' j', :as pos'] from-dirs]]
       (let [[a a' b b'] (if (#{:e :w} dir') [i i' j j'] [j j' i i'])]
         (and
          (= a a')
          (if (#{:n :w} dir') (< b' b) (< b b'))
          (some #{dir'} from-dirs)
          pos')))
     obs-hit-from)))

(defmethod advance :clear [_ {:keys [pos dir obs-hit-from], :as state}]
  (let [pos' (step pos dir)]
    (-> state
        (assoc :pos pos')
        (update :path #(conj % pos'))
        (update :loop-obs (fn [obs] (cond-> obs (will-loop-if-turning-right? pos dir obs-hit-from) (conj (step pos dir))))))))

(defmethod advance :obs [_ {:keys [pos dir], :as state}]
  (-> state
      (update-in [:obs-hit-from (step pos dir)] #(conj (or % #{}) dir))
      (update :dir turn-right)))

(defmethod advance :edge [_ state]
  (assoc state :finished? true))

(defn solve [m state]
  (let [advance (partial advance m)
        {:keys [path loop-obs]} (loop [state state] (if (:finished? state) state (recur (advance state))))]
    [(count (set path)) (count loop-obs)]))

(defn- parse [input]
  (let [m (char-matrix (str/split-lines input))
        pos (first (mfind m #(= \^ %)))]
    [m {:pos pos, :dir :n, :path [pos], :obs-hit-from {}}]))

(def ^:private test-input
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(apply solve (parse test-input))

(let [[m state] (parse (get-puzzle-input 24 6))]
  (println "Solutions:" (time (solve m state))))
