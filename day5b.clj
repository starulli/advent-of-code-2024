(ns adventofcode2024.day5
  (:require [clojure.string :refer [split]]
            util))

(defn parse-rules [s]
  (let [int (comp util/to-ints rest)]
    (->> s (re-seq #"(\d+)\|(\d+)") (map int) (into #{}))))

(defn parse-pages [s]
  (->> s (re-seq #"\d+") util/to-ints))

(defn parse-updates [s]
  (map parse-pages (split s #"\n")))

(defn parse-manual [filename]
  (let [[rules updates] (-> filename slurp (split #"\n\n"))]
    {:rules (parse-rules rules) :updates (parse-updates updates)}))

(defn rules-sort [rules pages]
  (vec (sort #(some? (rules [%1 %2])) pages)))

(defn page-eq [{:keys [:in :sorted]}]
  (= in sorted))

(defn middle-value [pages]
  (get pages (quot (count pages) 2)))

(let [{:keys [:updates :rules]} (parse-manual "inputs/day5.txt")
      sort (partial rules-sort rules)
      all (map #(hash-map :in % :sorted (sort %)) updates)
      q1 (->> all (filterv page-eq) (map (comp middle-value :in)) util/sum)
      q2 (->> all (filterv (complement page-eq)) (map (comp middle-value :sorted)) util/sum)]
  (println "Q1:" q1)
  (println "Q2:" q2))
