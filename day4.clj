(ns adventofcode2024.day4
  (:require [clojure.string :as str]
            [adventofcode2024.util :as util]))

(defn parse-grid [s]
  (into [] (map #(into [] %) (str/split s #"\n"))))

(defn parse-crossword [filename]
  (->> filename slurp parse-grid))

(defn vectors-nesw [dist]
  (let [adds (range 1 dist) subs (map - adds) vf #(vector % 0) vl #(vector 0 %)]
    (for [f [vl vf] coll [adds subs]]
      (map f coll))))

(defn vectors-diag [dist]
  (let [adds (range 1 dist) subs (map - adds)]
    (for [xs [adds subs] ys [adds subs]]
      (map vector xs ys))))

(defn vectors-8 [dist]
  (concat (vectors-nesw dist) (vectors-diag dist)))

(defn grid-get [grid [x y]]
  (get-in grid [y x] nil))

(defn search-space [grid word svf]
  {:grid grid :x (count (first grid)) :y (count grid)
   :anchor (first word) :letters (rest word) :vectors (svf (count word))})

(defn find-anchors [{:keys [:grid :x :y :anchor]}]
  (for [r (range y) c (range x) :let [p [c r]] :when (= (grid-get grid p) anchor)]
    p))

(defn pos-matches [pos {:keys [:grid :vectors :letters]}]
  (let [offset #(grid-get grid (map + pos %))
        match (fn [sv] (every? true? (map #(= (offset %1) %2) sv letters)))]
    (remove nil? (map #(if (match %) [pos %] nil) vectors))))

(defn find-matches [ss]
  (->> ss find-anchors (map #(pos-matches % ss))))

(defn first-offset [[pos [off]]]
  (map + pos off))

(defn midpoints [matches]
  (let [two? #(= (val %) 2)]
    (->> (apply concat matches) (map first-offset) util/tally (filter two?))))

(let [crossword (parse-crossword "inputs/day4.txt")
      q1 (->> (search-space crossword "XMAS" vectors-8)
              find-matches (map count) util/sum)
      q2 (->> (search-space crossword "MAS" vectors-diag)
              find-matches midpoints count)]
  (println "Q1:" q1)
  (println "Q2:" q2))
