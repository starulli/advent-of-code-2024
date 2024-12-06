(ns adventofcode2024.day5
  (:require [clojure.string :refer [split]]
            util))

(defn parse-rules [s]
  (let [int (comp util/to-ints rest)
        add (fn [s [k v]] (update s k conj v))]
    (->> s (re-seq #"(\d+)\|(\d+)") (map int) (reduce add {}))))

(defn parse-pages [s]
  (->> s (re-seq #"\d+") (mapv Integer/parseInt)))

(defn parse-updates [s]
  (map parse-pages (split s #"\n")))

(defn parse-manual [filename]
  (let [[rules updates] (-> filename slurp (split #"\n\n"))]
    {:rules (parse-rules rules) :updates (parse-updates updates)}))

(defn calc-page [deps pagedata]
  (let [flip (fn [[a b]] [b a])
        indices (into {} (map flip (util/indexed pagedata)))
        dep-indices #(->> % deps (mapv indices) (remove nil?) sort)]
    (mapv #(hash-map :index (indices %) :value % :deps (dep-indices %)) pagedata)))

(defn page-valid? [{:keys [:index :deps]}]
  (if-let [d (first deps)]
    (> d index)
    true))

(defn update-valid? [pages]
  (every? page-valid? pages))

(defn reconstruct [pages]
  (mapv :value pages))

(defn reconstitute [value from to v]
  (vec (concat (subvec v 0 to) [value] (subvec v to from) (subvec v (inc from)))))

(defn reorder [pages]
  (let [{:keys [:index :value :deps]} (first (remove page-valid? pages))]
    (reconstitute value index (first deps) (reconstruct pages))))

(defn fixup [rules pages]
  (loop [p pages]
    (if (update-valid? p)
      p
      (recur (->> p reorder (calc-page rules))))))

(defn middle-value [update]
  (get-in update [(quot (count update) 2) :value]))

(let [manual (parse-manual "inputs/day5.txt")
      rules (manual :rules)
      rcalc (partial calc-page rules)
      updates (->> :updates manual (map rcalc))
      q1 (->> updates (filter update-valid?) (map middle-value) util/sum)
      rfixup (partial fixup rules)
      fixes (comp middle-value rfixup)
      q2 (->> updates (filter (complement update-valid?)) (map fixes) util/sum)]
  (println "Q1:" q1)
  (println "Q2:" q2))
