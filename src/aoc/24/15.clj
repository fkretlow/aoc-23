(ns aoc.24.15
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mfind mget mset mshape v+]]
   [aoc.lib.seq :refer [find-first]]
   [clojure.string :as str]))


(def ^:private test-input
  "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")


(def ^:private north [-1 0])
(def ^:private east [0 1])
(def ^:private south [1 0])
(def ^:private west [0 -1])
(def ^:private dirs {\^ north, \> east, \v south, \< west})


(defn- parse [input]
  (let [[grid-str dir-str] (str/split input #"\n\n"),
        grid (char-matrix (str/split-lines grid-str))]
    {:grid grid
     :dirs (map dirs (filter #(not= \newline %) dir-str))
     :robot (first (mfind grid #(= \@ %)))}))


(defn- on-the-grid? [grid [i j]]
  (let [[h w] (mshape grid)] (and (< -1 i h) (< -1 j w))))


;; TODO push the next box, don't forget partially colliding boxes
(defn- push-double-box [grid p1 dir]
  (let [c (mget grid p1)
        p2 (case c \[ (v+ p1 east), \] (v+ p1 west) (throw (ex-info "pushing something weird" {:weird c})))
        [p'1 p'2] (sort-by second (map #(v+ dir %) [p1 p2]))
        can-push? (= \. (mget grid p'1) (mget grid p'2))]
    (if can-push?
      [(-> grid
           (mset p1 \.) (mset p2 \')
           (mset p'1 \[) (mset p'2 \]))
       true]
      [grid false])))


(defn- push
  "Given the grid and a point on the grid, attempt to push the box or the robot at the
  given position in the given direction. Returns the new grid and `true` if the push
  succeeded, `false` otherwise, as a pair."
  [grid pos dir]
  (assert (on-the-grid? grid pos) "attempt to push outside the grid")
  (let [c (mget grid pos), pos' (v+ pos dir)]
    (assert (#{\@ \O \[ \]} c) (format "attempt to push %c != @,O", c))
    (case (mget grid pos')
      \# [grid, false]
      \. [(-> grid (mset pos' c) (mset pos \.)), true]
      \O (let [[grid' success?] (push grid pos' dir)
               grid'' (if success? (-> grid' (mset pos' c) (mset pos \.)) grid')]
           [grid'' success?])
      (throw (ex-info "bumping into something funny" {:pos pos'})))))


(defn- step
  "Given the current state as a map containing the grid, the current position of the
  robot, and the remaining directions for the robot to move, advance the state one
  step by consuming one direction and moving the robot and pushing boxes accordingly.
  Returns the updated state.
  Does nothing and returns the original state if no directions are left."
  [{:keys [grid robot dirs], :as state}]
  (if-let [dir (first dirs)]
    (let [[grid' success?] (push grid robot dir)]
      (cond-> (update state :dirs rest)
        success? (assoc :grid grid')
        success? (update :robot #(v+ dir %))))
    state))


(defn ->gps [[i j]] (+ (* 100 i) j))


(defn part-1 [state]
  (let [{grid :grid} (find-first (comp empty? :dirs) (iterate step state))]
    (->> (mfind grid #(= \O %))
         (map ->gps)
         (reduce +))))


(let [state (parse (get-puzzle-input 24 15))]
  (println "Part 1:" (time (part-1 state))))
