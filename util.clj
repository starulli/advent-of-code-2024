(ns util)

(defn sum [coll]
  (reduce + 0 coll))

(defn tally [coll]
  (let [f (fnil inc 0)]
    (reduce #(update %1 %2 f) {} coll)))
