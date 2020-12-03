(ns day3.core
  (:gen-class))



(defn find-trees-in-path
  [lines {cols-skip :cols rows-skip :rows}]
  (let [mod-nth        (fn [coll n] (nth coll (mod n (count coll))))
        keep-every-nth (fn [n] (fn [index item] (when (= 0 (mod index n)) item)))
        is-tree?       (fn [c] (= \# c))]
    (->> lines
         (keep-indexed (keep-every-nth rows-skip))
         (reduce
           (fn [{:keys [tree-count current-col]} line]
             {:tree-count  (cond-> tree-count
                             (is-tree? (mod-nth line current-col)) inc)
              :current-col (+ current-col cols-skip)})
           {:tree-count  0
            :current-col 0})
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
