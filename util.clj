(ns util)

(defn sum [coll]
  (reduce + 0 coll))

(defn tally [coll]
  (let [f (fnil inc 0)]
    (reduce #(update %1 %2 f) {} coll)))

(defn indexed [coll]
  (map vector (range) coll))

(defn to-ints [coll]
  (mapv Integer/parseInt coll))
