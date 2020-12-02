(ns day2.core
  (:gen-class)
  (:require [clojure.string :as c.str]))

(defn valid-entry?
  [[rule password]]
  (let [[range letter]            (c.str/split rule #" ")
        [lower-bound upper-bound] (mapv #(Integer/parseInt %) (c.str/split range #"-"))
        freq                      (frequencies password)
        letter-count              (get freq (first letter) 0)]
    (<= lower-bound letter-count upper-bound)))

(defn count-valid-passwords
  [coll]
  (when-let [s (seq coll)]
    (let [line->entry-fn (fn [line] (c.str/split line #":"))
          valid-list     (filter (comp valid-entry? line->entry-fn) s)]
     (count valid-list))))

(defn -main
  "Run with cat resources/input.txt | lein run"
  [& args]
  (let [lines (c.str/split-lines (slurp *in*))]
    (print (count-valid-passwords lines))))
