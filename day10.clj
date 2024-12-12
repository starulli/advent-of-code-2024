(ns adventofcode2024.day10
  (:require [util :refer [split-lines to-ints indexed sum]]))

(defn unconnected-graph [nss]
  (letfn [(make [y g [x h]]
            (assoc g [y x] {:height h :edges []}))
          (parse-row [g [y row]]
            (reduce (partial make y) g (indexed row)))]
    (reduce parse-row {} (indexed nss))))

(defn connect-by-height-diff [g]
  (letfn [(neighbours [p]
            (map #(mapv + p %) [[-1 0] [0 1] [1 0] [0 -1]]))
          (gentle-incline? [p q]
            (= (:height p) (dec (:height q))))
          (edges [g p]
            (filter #(and (contains? g %) (gentle-incline? (g p) (g %))) (neighbours p)))
          (connect [g p]
            (assoc-in g [p :edges] (edges g p)))]
    (reduce connect g (keys g))))

(defn parse-graph [filename]
  (let [nss (->> filename slurp split-lines (map seq) (map to-ints))]
    (unconnected-graph nss)))

(defn trail-heads [g]
  (map key (filter #(-> % second :height zero?) g)))

(defn bfs [g root]
  (letfn [(traverse [path edges]
            (map #(conj path %) edges))]
    (loop [[path & rem] [[root]], results []]
      (let [v (-> path last g)]
        (cond (nil? path) results
              (empty? (:edges v)) (recur rem (conj results path))
              :follow-edges (recur (apply conj rem (traverse path (:edges v))) results))))))

(defn bfs-roots [g roots]
  (reduce #(assoc %1 %2 (bfs g %2)) {} roots))

(defn longest-paths [paths]
  (filter #(= (count %) 10) paths))

(defn unique-terminals [allpaths]
  (letfn [(ucount [paths]
            (->> paths longest-paths (map last) set count))]
    (reduce-kv #(assoc %1 %2 (ucount %3)) {} allpaths)))

(let [g (->> "inputs/day10.txt" parse-graph connect-by-height-diff)
      trails (->> g trail-heads (bfs-roots g))
      q1 (->> trails unique-terminals vals sum)
      q2 (->> trails vals (map (comp count longest-paths)) sum)]
  (println "Q1:" q1)
  (println "Q2:" q2))
