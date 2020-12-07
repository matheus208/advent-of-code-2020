(ns day6.core
  (:gen-class)
  (:require [clojure.string]
            [clojure.set]))

(defn group-answers
  [input]
  (clojure.string/split input #"\n\n"))

(defn individual-answers
  [raw-answers]
  )

(defn count-any-yes-from-group
  [group]
  (as-> group %
        (clojure.string/replace % #"\s" "")
        (into #{} %)
        (count %)))

(defn count-all-yes-from-group
  [group]
  (let [individual-answers-list (-> group
                                    (clojure.string/replace #" " "") ;the actual input doesnt have any spaces, but copy&pasting specific parts from the browser produces them
                                    (clojure.string/split #"\n"))]
    (->> individual-answers-list
         (map #(into #{} %))
         (reduce clojure.set/intersection)
         count)))

(defn count-yes-from-groups
  [input group-counting-fn]
  (->> input
       group-answers
       (map group-counting-fn)
       (reduce +)))

(defn -main
  [& args]
  (let [input (slurp *in*)]
    (println (count-yes-from-groups input count-any-yes-from-group))
    (println (count-yes-from-groups input count-all-yes-from-group))))
