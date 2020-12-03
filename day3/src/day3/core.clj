(ns day3.core
  (:gen-class))



(defn find-trees-in-path
  [lines strategy]
  (let [cols-skip (:cols strategy)
        rows-skip (:rows strategy)
        mod-nth   (fn [n coll] (nth coll (mod n (count coll))))]
    (->> lines
         (keep-indexed (fn [index item] (when (= 0 (mod index rows-skip)) item)))
         (reduce
           (fn [ctx line]
             (let [col (:col ctx)]
               (-> ctx
                   (update :tree-count (if (= \. (mod-nth col line))
                                         identity
                                         inc))
                   (update :col (partial + cols-skip)))))
           {:tree-count 0
            :col        0})
         :tree-count)))

(defn -main
  [& args]
  (let [lines (clojure.string/split-lines (slurp *in*))]
    (println (find-trees-in-path lines {:cols 3 :rows 1}))
    (println (->> [{:cols 1 :rows 1}
                   {:cols 3 :rows 1}
                   {:cols 5 :rows 1}
                   {:cols 7 :rows 1}
                   {:cols 1 :rows 2}]
                  (map #(find-trees-in-path lines %))
                  (apply *)))))
