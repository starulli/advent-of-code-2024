(ns adventofcode2024.day13
  (:require [util :refer [sum]]))

(defn parse-machines [filename]
  (->> filename slurp (re-seq #"\d+") (map bigint) (partition 6) (map vec)))

(defn cost [[a b]]
  (+ (* 3 a) b))

(defn presses [[ax ay bx by x y]]
  (let [b (/ (- (* ax y) (* ay x))
             (+ (* (- ay) bx) (* ax by)))
        a (/ (- x (* bx b)) ax)]
    [a b]))

(defn rational-answer? [[a b]]
  (or (ratio? a) (ratio? b)))

(defn adjust-prize [[ax ay bx by x y]]
  [ax ay bx by (+ 10000000000000 x) (+ 10000000000000 y)])

(defn tokens [machines]
  (->> machines (map presses) (remove rational-answer?) (map cost) sum))

(let [machines (parse-machines "inputs/day13.txt")
      q1 (tokens machines)
      q2 (->> machines (map adjust-prize) tokens)]
  (println "Q1:" q1)
  (println "Q2:" q2))
