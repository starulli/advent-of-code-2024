(ns adventofcode2024.day11
  (:require [util :refer [to-ints tally sum]]))

(defn parse-stones [filename]
  (->> filename slurp (re-seq #"\d+") to-ints))

(def even-digits? (comp even? count str))

(defn split-digits [n]
  (let [s (str n), len (-> s count (/ 2))]
    (to-ints [(subs s 0 len) (subs s len)])))

(defn transmute [stone]
  (cond (zero? stone) [1]
        (even-digits? stone) (split-digits stone)
        :else [(* stone 2024)]))

(defn transmogriphy [stones-map]
  (reduce-kv (fn [m k v]
               (let [agg (fnil #(+ % v) 0), up #(update %1 %2 agg)]
                 (->> k transmute (reduce up m))))
             {} stones-map))

(let [stones-map (->> "inputs/day11.txt" parse-stones tally)
      q1 (->> stones-map (iterate transmogriphy) (drop 25) first vals sum)
      q2 (->> stones-map (iterate transmogriphy) (drop 75) first vals sum)]
  (println "Q1:" q1)
  (println "Q2:" q2))
