(ns adventofcode2024.day14
  (:require [util :refer [tally to-ints]]))

(defn parse-robots [filename]
  (->> filename slurp (re-seq #"-?\d+") to-ints (partition 4) (map vec)))

(defn simulate [w h secs [px py vx vy]]
  (let [x (mod (+ (* secs vx) px) w)
        y (mod (+ (* secs vy) py) h)]
    [x y]))

(defn make-quadrant [w h]
  (let [midx (quot w 2), midy (quot h 2)]
    (fn [[x y]]
      (cond (and (< -1 x midx) (< -1 y midy)) 1
            (and (< midx x w) (< -1 y midy))  2
            (and (< -1 x midx) (< midy y h))  3
            (and (< midx x w) (< midy y h))   4
            :else nil))))

(defn draw-robots [w h robots]
  (letfn [(draw-cell [v]
            (if (nil? v) "." (str v)))
          (draw-row [r]
            (apply str (map #(draw-cell (r %)) (range 0 w))))
          (ys [[_ y]] y)
          (xs [coll] (->> coll (map first) tally))]
    (let [rows (->> robots (group-by ys) (reduce-kv #(assoc %1 %2 (xs %3)) {}))
          all (merge (into {} (map #(vector % {}) (range 0 h))) rows)
          drawables (->> all keys sort (map #(draw-row (all %))))]
      (doall (map println drawables)))))

(let [filename "inputs/day14.txt"
      robots (parse-robots filename)
      [w h] (if (clojure.string/includes? filename "example") [11 7] [101 103])
      sim (partial simulate w h)
      quadrant (make-quadrant w h)
      q1 (->> robots (map (partial sim 100)) (map quadrant) (remove nil?) tally vals (reduce * 1))
      q2 (first (for [secs (range 0 10000)
                      :let [ps (map (partial sim secs) robots)]
                      :when (= (count ps) (count (set ps)))]
                  [secs ps]))
      draw-q2 false]
  (println "Q1:" q1)
  (println "Q2:" (first q2))
  (when draw-q2
    (draw-robots w h (second q2))))
