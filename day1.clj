(ns adventofcode2024.day1)

(defn read-number-lists [filename]
  (->> filename slurp (re-seq #"\d+") (map Integer/parseInt)))

(defn alternating-partition [xs]
  (let [indexed (map vector (range) xs)]
    (reduce (fn [lr [i x]]
              (let [side (if (even? i) 0 1)]
                (update lr side #(conj % x))))
            [[] []] indexed)))

(defn sort-all [xs]
  (map sort xs))

(defn calculate-distance-list [[as bs]]
  (map #(abs (- %1 %2)) as bs))

(defn sum [xs]
  (reduce + 0 xs))

(defn tally [xs]
  (let [f (fnil inc 0)]
    (reduce #(update %1 %2 f) {} xs)))

(defn calculate-tally-list [[as bs]]
  [as (tally bs)])

(defn calculate-similarity-scores [[as ts]]
  (map #(* % (get ts % 0)) as))

(let [numbers (-> "inputs/day1.txt" read-number-lists alternating-partition)
      q1 (-> numbers sort-all calculate-distance-list sum)
      q2 (-> numbers calculate-tally-list calculate-similarity-scores sum)]
  (println "Q1:" q1)
  (println "Q2:" q2))
