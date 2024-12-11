(ns adventofcode2024.day9
  (:require util))

(defn make-node [id kind n]
  (repeat n (if (= kind :file) (swap! id inc) :free)))

(defn parse-disk-map [filename]
  (let [ns (->> filename slurp (re-seq #"\d") (map Integer/parseInt))
        make (partial make-node (atom -1))
        nodes (map make (cycle [:file :free]) ns)]
    (->> nodes flatten (into []))))

(defn split-compaction [dm]
  (loop [i 0, j (dec (count dm)), dm dm]
    (cond (>= i j) dm
          (not= (dm i) :free) (recur (inc i) j dm)
          (= (dm j) :free) (recur i (dec j) dm)
          :flip (recur i j (assoc dm j :free i (dm j))))))

(defn end-index [dm i f pred]
  (if (or (zero? i) (-> i dm pred not))
    i
    (recur dm (f i) f pred)))

(defn find-free [dm n end]
  (loop [i 0]
    (cond (>= i end) nil
          (not= (dm i) :free) (recur (inc i))
          :else (if-let [j (end-index dm i inc #(= % :free))]
                  (if (>= (- j i) n) [i j] (recur j))
                  nil))))

(defn assoc-kvs [v1 [i1 i2] v2 [j1 j2]]
  (let [ivals (map vector (range i1 i2) (repeat v1))
        jvals (map vector (range (inc j1) (inc j2)) (repeat v2))]
    (flatten (concat ivals jvals))))

(defn block-compaction [dm]
  (loop [j (dec (count dm)), dm dm]
    (cond (zero? j) dm
          (= (dm j) :free) (recur (dec j) dm)
          :flipall (let [j2 (end-index dm j dec #(= % (dm j)))]
                     (if-let [[i i2] (find-free dm (- j j2) j2)]
                       (let [ie (+ i (min (- j j2) (- i2 i)))]
                         (recur j (apply assoc dm (assoc-kvs (dm j) [i ie] :free [j2 j]))))
                       (recur j2 dm))))))

(defn checksum [dm]
  (->> dm util/indexed (remove #(= (second %) :free)) (map #(apply * %)) util/sum))

(let [dm (parse-disk-map "inputs/day9.txt")
      q1 (->> dm split-compaction checksum)
      q2 (->> dm block-compaction checksum)]
  (println "Q1:" q1)
  (println "Q2:" q2))
