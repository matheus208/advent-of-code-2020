(ns day5.core
  (:gen-class)
  (:require [clojure.set]
            [clojure.string]))

(def seat-rows 127)
(def seats-in-a-row 7)

(defn round-up [i] (int (Math/ceil i)))
(defn round-down [i] (int (Math/floor i)))

(defn binary-partition
  [instructions n]
  (reduce
    (fn [[lower-bound upper-bound] instruction]
      (let [half (/ (- upper-bound lower-bound)
                    2)]
        (if (contains? #{\F \L} instruction)
          [lower-bound (round-down (+ lower-bound half))]
          [(round-up (+ lower-bound half)) upper-bound])))
    [0 n]
    instructions))

(defn seat-row
  [seat]
  (let [seat-row-partitioning (take 7 seat)]
    (first (binary-partition seat-row-partitioning seat-rows))))

(defn seat-col
  [seat]
  (let [seat-col-partitioning (drop 7 seat)]
    (first (binary-partition seat-col-partitioning seats-in-a-row))))

(defn seat-coordinates
  [seat]
  [(seat-row seat)
   (seat-col seat)])

(defn seat-coord->seat-id
  [[row col]]
  (+ col (* row 8)))
(defn seat-id
  [seat]
  (seat-coord->seat-id (seat-coordinates seat)))

(defn find-highest-seat-id
  [seats]
  (apply max (map seat-id seats)))

(defn find-missing-seats
  [seats]
  (let [input-seat-ids    (map seat-id seats)
        max-input-seat-id (apply max input-seat-ids)
        min-input-seat-id (apply min input-seat-ids)
        aircraft-seat-ids (->> (for [row (range seat-rows)
                                     col (range seats-in-a-row)]
                                  (seat-coord->seat-id [row col]))
                                (filter #(<= min-input-seat-id % max-input-seat-id)))]
    (clojure.set/difference (set aircraft-seat-ids)
                            (set input-seat-ids))))

(defn -main
  [& args]
  (let [seats (clojure.string/split-lines (slurp *in*))]
    (println (find-highest-seat-id seats))
    (println (find-missing-seats seats))))
