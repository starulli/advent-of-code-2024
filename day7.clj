(ns adventofcode2024.day7
  (:require util))

(defn parse-line [l]
  (let [[answer & terms] (util/to-big-ints (re-seq #"\d+" l))]
    {:answer answer :terms terms}))

(defn parse-tests [filename]
  (->> filename slurp util/split-lines (mapv parse-line)))

(defn apply-ops [ops {:keys [:total :terms :path]}]
  (let [[t & ts] terms]
    (map #(hash-map :total (% total t) :terms ts :path (conj path % t)) ops)))

(defn calibration [ops {:keys [:answer :terms]}]
  (let [[t & ts] terms
        f (partial apply-ops ops)]
    (loop [[state & rem] [{:total t :terms ts :path [t]}] found []]
      (cond (nil? state) {:answer answer :paths (map :path found)}
            (> (:total state) answer) (recur rem found)
            (seq (:terms state)) (recur (apply conj rem (f state)) found)
            (= (:total state) answer) (recur rem (conj found state))
            :discard (recur rem found)))))

(def paths? (comp not-empty :paths))

(defn || [a b]
  (bigint (str a b)))

(defn calibration-result [calif tests]
  (->> tests (map calif) (filter paths?) (map :answer) util/sum))

(let [tests (parse-tests "inputs/day7.txt")
      q1 (calibration-result (partial calibration [+ *]) tests)
      q2 (calibration-result (partial calibration [+ * ||]) tests)]
  (println "Q1:" (str q1))
  (println "Q2:" (str q2)))
