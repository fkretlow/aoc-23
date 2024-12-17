(ns aoc.24.17
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.seq :refer [find-first]]
   [clojure.math :refer [pow]]
   [clojure.string :as str]))

(def ^:dynamic *log* false)
(defn- log [& args] (when *log* (apply println args)))

(defn- parse [input]
  (let [lines (str/split-lines input)
        [A B C] (map (comp first parse-longs) (take 3 lines))
        program (parse-longs (nth lines 4))]
    {:registers {:A A, :B B, :C C},
     :program program,
     :iptr 0,
     :halted? false,
     :stdout []}))

(def ^:private opcodes [:adv :bxl :bst :jnz :bxc :out :bdv :cdv])

(defn- ->opc [n]
  (or (nth opcodes n) (throw (ex-info "invalid opcode" {:opcode n}))))

(defmulti tick (fn [{:keys [iptr program]}]
                 (if (< -1 iptr (count program))
                   (->opc (nth program iptr))
                   :halt)))

(defn- eval-cop [{:keys [A B C]} cop]
  (case cop 0 0, 1 1, 2 2, 3 3, 4 A, 5 B, 6 C, (throw (ex-info "invalid combo operand" {:operand cop}))))

(defmethod tick :adv [{:keys [registers iptr program], :as state}]
  (log iptr "adv")
  (let [cop (nth program (inc iptr))
        den (pow 2 (eval-cop registers cop))
        num (-> registers :A)]
    (-> state
        (assoc-in [:registers :A] (int (/ num den)))
        (update :iptr #(+ 2 %)))))

(defmethod tick :bxl [{:keys [iptr program], :as state}]
  (log iptr "bxl")
  (let [lop (nth program (inc iptr))]
    (-> state
        (update-in [:registers :B] bit-xor lop)
        (update :iptr #(+ 2 %)))))

(defmethod tick :bst [{:keys [registers iptr program], :as state}]
  (log iptr "bst")
  (let [cop (nth program (inc iptr))]
    (-> state
        (assoc-in [:registers :B] (mod (eval-cop registers cop) 8))
        (update :iptr #(+ 2 %)))))

(defmethod tick :jnz [{:keys [iptr program registers], :as state}]
  (log iptr "jnz")
  (if (zero? (-> registers :A))
    (update state :iptr #(+ 2 %))
    (assoc state :iptr (nth program (inc iptr)))))

(defmethod tick :bxc [{:keys [registers iptr], :as state}]
  (log iptr "bxc")
  (let [{:keys [B C]} registers]
    (-> state
        (assoc-in [:registers :B] (bit-xor B C))
        (update :iptr #(+ 2 %)))))

(defmethod tick :out [{:keys [registers iptr program], :as state}]
  (log iptr "out")
  (let [cop (nth program (inc iptr))]
    (-> state
        (update :stdout #(conj % (mod (eval-cop registers cop) 8)))
        (update :iptr #(+ 2 %)))))

(defmethod tick :bdv [{:keys [registers iptr program], :as state}]
  (log iptr "bdv")
  (let [cop (nth program (inc iptr))
        den (pow 2 (eval-cop registers cop))
        num (-> registers :A)]
    (-> state
        (assoc-in [:registers :B] (int (/ num den)))
        (update :iptr #(+ 2 %)))))

(defmethod tick :cdv [{:keys [registers iptr program], :as state}]
  (log iptr "cdv")
  (let [cop (nth program (inc iptr))
        den (pow 2 (eval-cop registers cop))
        num (-> registers :A)]
    (-> state
        (assoc-in [:registers :C] (int (/ num den)))
        (update :iptr #(+ 2 %)))))

(defmethod tick :halt [{:keys [iptr], :as state}]
  (log iptr "halt")
  (assoc state :halted? true))

(defn run [state]
  (find-first :halted? (take 1000 (iterate tick state))))

(let [state {:registers {:C 9}, :iptr 0 :program [2 6], :stdout []}]
  (assert (= 1 (-> (run state) :registers :B))))

(let [state {:registers {:A 10}, :iptr 0, :program [5 0 5 1 5 4], :stdout []}]
  (assert (= [0 1 2] (-> (run state) :stdout))))

(let [state {:registers {:A 2024}, :iptr 0, :program [0 1 5 4 3 0], :stdout []}
      {:keys [registers stdout]} (run state)]
  (assert (= 0 (-> registers :A)))
  (assert (= [4 2 5 6 7 7 7 7 3 1 0] stdout)))

(let [state {:registers {:B 29}, :iptr 0, :program [1 7], :stdout []}]
  (assert (= 26 (-> state run :registers :B))))

(let [state {:registers {:B 2024, :C 43690}, :iptr 0, :program [4 0]}]
  (assert (= 44354 (-> state run :registers :B))))

(defn part-1 [state]
  (->> state run :stdout (str/join ",")))

(defn reproduces-itself? [{:keys [program], :as state}]
  (loop [state state]
    (let [{:keys [stdout halted?], :as state'} (tick state)]
      (cond
        (empty? stdout) (recur state')
        (not= stdout (take (count stdout) program)) false
        (and halted? (= stdout program)) true
        :else (recur state')))))

(defn part-2 [state]
  (find-first reproduces-itself? (map #(assoc-in state [:registers :A] %) (range 117442))))

(def ^:private test-input
  "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0")

(let [state (parse (get-puzzle-input 24 17))]
  (println "Part 1:" (time (part-1 state))))
