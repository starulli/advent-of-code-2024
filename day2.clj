(ns adventofcode2024.day2
  (:require [clojure.string :as str]))

(defn read-levels [line]
  (->> line (re-seq #"\d+") (map Integer/parseInt) (into [])))

(defn read-reports [bytes]
  (str/split bytes #"\n"))

(defn parse-reports [filename]
  (->> filename slurp read-reports (map read-levels)))

(defn delta-seq [coll]
  (->> coll (partition 2 1) (map #(apply - %))))

(defn monotonic? [coll]
  (or (apply < coll) (apply > coll)))

(defn safe? [report]
  (and (monotonic? report) (every? #(<= 1 (abs %) 3) (delta-seq report))))

(defn remove-index [i coll]
  (into (subvec coll 0 i) (subvec coll (inc i))))

(defn dampened-seq [coll]
  (->> coll count range (map #(remove-index % coll))))

(defn problem-dampener [coll]
  (concat [coll] (dampened-seq coll)))

(let [reports (->> "inputs/day2.txt" parse-reports)
      q1 (->> reports (filter safe?) count)
      q2 (->> reports (filter #(some safe? (problem-dampener %))) count)]
  (println "Q1:" q1)
  (println "Q2:" q2))
