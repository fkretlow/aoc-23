(ns aoc.24.03
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]))

; We define a little state machine.
; The state is a map containing the current sum as accumulation value, and, for part 2, whether we should
; skip mul instructions, e.g. `{:acc 0, :skip? false}`
; Events are vectors containing their type followed by possible payloads, e.g. `[:do]`, `[:don't]`, or `[:mul 1 2]`
; The multimethod `process` accepts the state and one event and returns the next state.
; Parsing is very easy with regular expressions.

(defmulti process (fn [_state [type, :as _event]] type))
(defmethod process :mul [state [_ x y]] (if (:skip? state) state (update state :acc #(+ % (* x y)))))
(defmethod process :do [state _] (assoc state :skip? false))
(defmethod process :don't [state _] (assoc state :skip? true))

(defn parse-events [{:keys [skips?], :as _options} input]
  (let [pattern (if skips?
                  #"(?:mul\(\d{1,3},\d{1,3}\))|(?:do\(\))|(?:don't\(\))"
                  #"mul\(\d{1,3},\d{1,3}\)")]
    (for [match (re-seq pattern input)]
      (cond
        (str/starts-with? match "mul")
        (vec (conj (parse-longs match) :mul))

        (str/starts-with? match "don't")
        [:don't]

        (str/starts-with? match "do")
        [:do]

        :else
        (throw (ex-info "funny match" {:match match}))))))

(defn part-1 [input]
  (let [events (parse-events {:skips? false} input)]
    (-> (reduce process {:acc 0} events)
        :acc)))

(defn part-2 [input]
  (let [events (parse-events {:skips? true} input)]
    (-> (reduce process {:acc 0, :skip? false} events)
        :acc)))

(let [input (get-puzzle-input 24 3)]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
