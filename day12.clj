(ns adventofcode2024.day12
  (:require [util :refer [indexed split-lines sum]]))

(defn parse-garden [filename]
  (letfn [(parse-row [[y row]]
            (map #(vector [y %1] %2) (range) row))]
    (->> filename slurp split-lines indexed (mapcat parse-row) (into {}))))

(defn connected-graph [vertices]
  (letfn [(neighbours [p]
            (map #(mapv + p %) [[-1 0] [0 1] [1 0] [0 -1]]))]
    (reduce-kv #(assoc %1 %2 {:plant %3 :edges (neighbours %2)}) {} vertices)))

(defn eq [g a b]
  (= (-> a g :plant) (-> b g :plant)))

(defn eq-region [g a]
  (letfn [(eq-edges [b]
            (filter #(eq g a %) (-> b g :edges)))]
    (loop [[k & ks] (eq-edges a), visited #{a}, result [a]]
      (cond (nil? k) result
            (visited k) (recur ks visited result)
            :else (recur (concat ks (eq-edges k)) (conj visited k) (conj result k))))))

(defn regions [g]
  (loop [[[k v] & ks] g, visited #{}, result {}]
    (cond (nil? k) result
          (visited k) (recur ks visited result)
          :else (let [reg (eq-region g k)]
                  (recur ks (apply conj visited reg) (assoc result k reg))))))

(defn perimeter [g reg]
  (letfn [(f [v]
            (map #(if (eq g v %) 0 1) (-> v g :edges)))]
    (->> reg (mapcat f) sum)))

(defn discount-perimeter [g reg]
  (letfn [(neq [a b] (not (eq g a b)))
          (up [[y x]] [(dec y) x])
          (down [[y x]] [(inc y) x])
          (left [[y x]] [y (dec x)])
          (right [[y x]] [y (inc x)])]
    (let [fs [#(and (neq (up %) %) (neq (left %) %))
              #(and (neq (up %) %) (neq (right %) %))
              #(and (neq (down %) %) (neq (left %) %))
              #(and (neq (down %) %) (neq (right %) %))
              #(and (eq g (up %) %) (eq g (left %) %) (neq (left (up %)) %))
              #(and (eq g (up %) %) (eq g (right %) %) (neq (right (up %)) %))
              #(and (eq g (down %) %) (eq g (left %) %) (neq (left (down %)) %))
              #(and (eq g (down %) %) (eq g (right %) %) (neq (right (down %)) %))]
          mapf (fn [a] (map #(if (% a) 1 0) fs))]
      (->> reg (mapcat mapf) sum))))

(let [g (->> "inputs/day12.txt" parse-garden connected-graph)
      kregions (-> g regions vals)
      areas (map count kregions)
      q1 (sum (map * areas (map (partial perimeter g) kregions)))
      q2 (sum (map * areas (map (partial discount-perimeter g) kregions)))]
  (println "Q1:" q1)
  (println "Q2:" q2))
