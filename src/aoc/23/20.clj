(ns aoc.23.20
  (:refer-clojure :exclude [name type])
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.queue :refer [queue]]
   [clojure.string :as str]))


(defmulti #^{:private true} process :type)
(defmethod process :default [_ _] nil)


(defmethod process :flip-flop [{:keys [name destinations state]} [_ _ pulse]]
  (case pulse
    1 nil
    0 (let [pulse' (if (swap! state not) 1 0)]
        (for [to destinations] [name to pulse']))))


(defmethod process :conjunction [{:keys [name destinations memory]} [input _ pulse]]
  (let [pulse' (if (apply = (cons 1 (vals (swap! memory #(assoc % input pulse))))) 0 1)]
    (for [to destinations] [name to pulse'])))


(defmethod process :broadcaster [{:keys [name destinations]} [_ _ pulse]]
  (for [to destinations] [name to pulse]))


(defn- ->module [line]
  (let [[name destinations] (str/split line #" -> ")
        type (case (first name)
               \% :flip-flop,
               \& :conjunction,
               :broadcaster)
        name (if (= type :broadcaster) name (subs name 1))
        destinations (str/split destinations #", ")]
    (cond-> {:name name,
             :type type,
             :destinations destinations}
      (= :flip-flop type) (assoc :state (atom false))
      (= :conjunction type) (assoc :memory (atom {})))))


(defn- ->system [lines]
  (let [modules (into {} (for [{name :name, :as m} (map ->module lines)] [name m]))]
    (doseq [from (vals modules),
            to (:destinations from),
            :let [{:keys [type memory]} (get modules to)]]
      (when (= :conjunction type) (swap! memory #(assoc % (:name from) 0))))
    {:modules modules, :log (atom nil), :queue (atom (queue))}))


(defn- push-button [system]
  (let [Q (:queue system)]
    (swap! Q #(conj % ["button" "broadcaster" 0]))
    (while (peek @Q)
      (let [[_ to, :as signal] (peek @Q)
            module (get-in system [:modules to])
            signals (process module signal)]
        (swap! (:log system) #(conj % signal))
        (swap! Q #(apply conj (pop %) signals))))))


(defn part-1 [system]
  (dotimes [_ 1000] (push-button system))
  (apply * (vals (frequencies (map last @(:log system))))))


(let [system (->system (str/split-lines (get-puzzle-input 23 20)))]
  (println "Part 1:" (time (part-1 system))))


;; Part 2 was solved by finding another least common multiple... No general solution provided here.
