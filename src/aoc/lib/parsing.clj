(ns aoc.lib.parsing)


(defn parse-longs
  "Return a lazy seq of all the integers in the string `s`, e.g. `\"a 12 b 3.4\"` -> `(12 3 4)`"
  [s] (map parse-long (re-seq #"-?\d+" s)))


(defn re-seq-pos
  "Find all matches of the given pattern in the given string and return
  them as a seq, including start and end positions. Credits to A. Webb,
  (https://stackoverflow.com/questions/21191045/get-string-indices-from-the-result-of-re-seq)."
  [p s]
  (let [m (re-matcher p s)]
    ((fn step []
       (when (.find m)
         (cons {:start (.start m), :end (.end m), :group (.group m)}
               (lazy-seq (step))))))))

