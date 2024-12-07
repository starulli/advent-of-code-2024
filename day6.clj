(ns adventofcode2024.day6
  (:require util))

(defn parse-map-char [[x c]]
  (let [xv (vector x (case c \# :obstruction :blank))
        g (= c \^)]
    [xv g]))

(defn parse-map-line [y line]
  (let [xvs (map parse-map-char (util/indexed line))
        guard (if-let [gx (->> xvs (filter (comp true? second)) ffirst first)]
                [y gx])
        yxvs (map (fn [[[x v] _]] [[y x] v]) xvs)]
    {:line yxvs :guard guard}))

(defn parse-map [filename]
  (let [lines (->> filename slurp util/split-lines (map parse-map-line (range)))
        guard (->> lines (map :guard) (filter some?) first)
        space (->> lines (mapcat :line) (into {}))]
    {:space space :guard guard :face [-1 0]}))

(defn rotate [[y x]]
  (if (zero? y) [x y] [x (- y)]))

(defn patrol [{:keys [:space :guard :face]}]
  (loop [pos guard dir face path [] visit #{}]
    (let [ve [pos dir] spot (space pos)]
      (cond
        (visit ve) :cycle
        (nil? spot) path
        (= :blank spot) (recur (mapv + pos dir) dir (conj path pos) (conj visit ve))
        :obstructed (recur (mapv - pos dir) (rotate dir) path visit)))))

(defn blocking-sim [in verts]
  (let [f #(patrol (assoc-in in [:space %] :obstruction))]
    (map f verts)))

(let [in (parse-map "inputs/day6.txt")
      verts (->> in patrol (into #{}))
      q1 (->> verts count)
      q2 (->> verts (blocking-sim in) (filter #(= % :cycle)) count)]
  (println "Q1:" q1)
  (println "Q2:" q2))
