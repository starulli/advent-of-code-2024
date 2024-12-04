(ns adventofcode2024.day3)

(def re-mul #"mul\(\d{1,3},\d{1,3}\)")
(def re-ifs #"(?:do|don't)\(\)|mul\(\d{1,3},\d{1,3}\)")

(defn parse-mul [inst]
  [:mul (map Integer/parseInt (re-seq #"\d+" inst))])

(defn parse-ifs [inst]
  (case inst
    "do()" [:do]
    "don't()" [:dont]
    (parse-mul inst)))

(defn sum-product [sum [_ args]]
  (+ (apply * args) sum))

(defn sum-if-product [{:keys [:sum :do] :as state} [cmd :as inst]]
  (cond (= cmd :do) (assoc state :do true)
        (= cmd :dont) (assoc state :do false)
        do (assoc state :sum (sum-product sum inst))
        :else state))

(def mul-eval (partial reduce sum-product 0))
(def ifs-eval (partial reduce sum-if-product {:sum 0 :do true}))

(let [program (slurp "inputs/day3.txt")
      q1 (->> program (re-seq re-mul) (map parse-mul) mul-eval)
      q2 (->> program (re-seq re-ifs) (map parse-ifs) ifs-eval :sum)]
  (println "Q1:" q1)
  (println "Q2:" q2))
