(ns aoc.24.09
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [data.deque :refer [add-first add-last deque peek-first peek-last
                       remove-first remove-last]]))


(defn parse
  "Parse a string of digits into a sequence of blocks of the form `[width id-or-nil]`.
  Returns the blocks as a deque for convenient further processing."
  [input]
  (loop [d (deque)
         lengths (map (comp parse-long str) (seq input))
         is-file? true
         file-id 0]
    (if (seq lengths)
      (recur (add-last d [(first lengths) (when is-file? file-id)])
             (rest lengths)
             (not is-file?)
             (cond-> file-id is-file? inc))
      d)))

(defn- is-file? [[_ content]] (some? content))

(defn move-one-block [[disk disk']]
  (let [[w-left :as block-left] (peek-first disk)
        [w-right content-right, :as block-right] (peek-last disk)]
    (cond
      (is-file? block-left)
      [(remove-first disk) (conj disk' block-left)]

      (not (is-file? block-right))
      [(remove-last disk) disk']

      (< w-left w-right)
      [(-> disk
           (remove-first)
           (remove-last)
           (add-last [(- w-right w-left) content-right]))
       (conj disk' [w-left content-right])]

      (= w-left w-right)
      [(-> disk (remove-first) (remove-last)) (conj disk' block-right)]

      (> w-left w-right)
      [(-> disk
           (remove-first)
           (add-first [(- w-left w-right) nil])
           (remove-last))
       (conj disk' block-right)])))

(defn part-1 [disk]
  (let [disk' (loop [[disk disk', :as state] [disk []]]
                (if (seq disk)
                  (recur (move-one-block state))
                  disk'))]
    (->> disk'
         (mapcat (fn [[w id]] (repeat w id)))
         (map-indexed (fn [i id] (* i id)))
         (reduce +))))

(let [disk (parse (get-puzzle-input 24 9))]
  (println "Part 1:" (time (part-1 disk))))

