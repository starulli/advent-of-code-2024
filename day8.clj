(ns adventofcode2024.day8
  (:require util))

(defn antennae [h w rows]
  (for [y (range (inc h)) x (range (inc w))
        :let [pos [y x] sig (get-in rows pos)]
        :when (not= \. sig)]
    [sig pos]))

(defn parse-antennae [h w rows]
  (let [as (antennae h w rows)
        add (fnil conj #{})
        agg (fn [m [k v]] (update m k add v))]
    (reduce agg {} as)))

(defn parse-signal-space [filename]
  (let [rows (->> filename slurp util/split-lines (map vec) vec)
        w (-> rows first count dec)
        h (-> rows count dec)]
    {:width w :height h :antennae (parse-antennae h w rows)}))

(defn slopes [points]
  (for [p1 points p2 (disj points p1)]
    {:a p1 :b p2 :m (mapv #(- %1 %2) p1 p2)}))

(defn bounded? [height width [y x]]
  (and (<= 0 y height) (<= 0 x width)))

(defn antinodes [f {:keys [:width :height :antennae]}]
  (let [pairs (mapcat slopes (vals antennae))
        pred (partial bounded? height width)]
    (into #{} (mapcat (partial f pred) pairs))))

(defn next-ends [pred {:keys [:a :b :m]}]
  (filter pred [(mapv + a m) (mapv - b m)]))

(defn bounded-line [pred {:keys [:a :b :m]}]
  (concat (take-while pred (iterate #(mapv + % m) a))
          (take-while pred (iterate #(mapv - % m) b))))

(let [space (parse-signal-space "inputs/day8.txt")
      q1 (count (antinodes next-ends space))
      q2 (count (antinodes bounded-line space))]
  (println "Q1:" q1)
  (println "Q2:" q2))
