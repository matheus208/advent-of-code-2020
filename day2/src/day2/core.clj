(ns day2.core
  (:gen-class)
  (:require [clojure.string :as c.str]))

(defn valid-entry-according-to-old-job?
  [[rule password]]
  (let [[range letter]            (c.str/split rule #" ")
        [lower-bound upper-bound] (mapv (comp #(Integer/parseInt %)) (c.str/split range #"-"))
        freq                      (frequencies password)
        letter-count              (get freq (first letter) 0)]
    (<= lower-bound letter-count upper-bound)))

(defn valid-entry-according-to-new-job?
  [[rule password]]
  (let [[positions letter]                                   (c.str/split rule #" ")
        char-letter                                          (first letter)
        [first-candidate-position second-candidate-position] (mapv (comp dec #(Integer/parseInt %)) (c.str/split positions #"-"))
        xor                                                  (fn [x y] (or (and x (not y)) (and (not x) y)))
        safe-nth                                             (fn [string pos] (when (<= 0 pos (count string))
                                                                               (nth string pos)))]
    (xor (= char-letter (safe-nth password first-candidate-position))
         (= char-letter (safe-nth password second-candidate-position)))))

(defn count-valid-passwords
  [coll validation-fn]
  (when-let [s (seq coll)]
    (let [line->entry-fn (fn [line] (c.str/split line #": "))
          valid-list     (filter (comp validation-fn  line->entry-fn) s)]
     (count valid-list))))

(defn -main
  "Run with cat resources/input.txt | lein run"
  [& args]
  (let [lines (c.str/split-lines (slurp *in*))]
    (println (count-valid-passwords lines valid-entry-according-to-old-job?))
    (println (count-valid-passwords lines valid-entry-according-to-new-job?))))
