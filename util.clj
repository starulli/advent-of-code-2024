(ns util
  (:require [clojure.string :as s]))

(defn sum [coll]
  (reduce + 0 coll))

(defn tally [coll]
  (let [f (fnil inc 0)]
    (reduce #(update %1 %2 f) {} coll)))

(defn indexed [coll]
  (map vector (range) coll))

(defn split-lines [str]
  (s/split str #"\n"))

(defn to-ints [coll]
  (mapv (comp Integer/parseInt str) coll))

(defn to-big-ints [coll]
  (mapv bigint coll))
