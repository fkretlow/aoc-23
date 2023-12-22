(ns aoc.lib.fun)


(defn validate [validate-fn f]
  (fn [& args]
    (let [result (apply f args)]
      (validate-fn args result)
      result)))

