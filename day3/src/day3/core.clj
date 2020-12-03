(ns day3.core
  (:gen-class))

(defn find-trees-in-path
  "Given a collection of lines and a traversing strategy, find how many trees there are on the path.
  The strategy map must contain :rows-skip and :cols-skip keys, which are the amount of rows/cols to skip on each hop to reach the next point in the path."
  [lines {:keys [cols-skip rows-skip]}]
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
    (println (find-trees-in-path lines {:cols-skip 3 :rows-skip 1}))
    (println (->> [{:cols-skip 1 :rows-skip 1}
                   {:cols-skip 3 :rows-skip 1}
                   {:cols-skip 5 :rows-skip 1}
                   {:cols-skip 7 :rows-skip 1}
                   {:cols-skip 1 :rows-skip 2}]
                  (map #(find-trees-in-path lines %))
                  (apply *)))))
