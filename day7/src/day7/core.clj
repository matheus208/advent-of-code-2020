(ns day7.core
  (:gen-class)
  (:require [clojure.string]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(def input (clojure.string/split-lines "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."))

(def input2 (clojure.string/split-lines "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."))


(defn input->dependency-map
  [acc-map line]
  (let [processed-str          (-> line
                                   (clojure.string/replace #" bags" "")
                                   (clojure.string/replace #" bag" "")
                                   (clojure.string/replace "." ""))
        [container containees] (clojure.string/split processed-str #" contain ")
        containees-list        (if (= "no other" containees)
                                 []
                                 (->> (clojure.string/split containees #", ")
                                      (map #(clojure.string/split % #" " 2))))]
    (reduce (fn [m [quantity colour]]
              (update m
                      container
                      #(conj % {:quantity (Integer/valueOf quantity), :colour colour})))
            acc-map
            containees-list)))

(defn dependency-map->ubergraph
  [dependency-map]
  (let [ubergraph-containee-to-container-input (mapcat
                                                 (fn [[container-bag v]]
                                                   (map (fn [{quantity :quantity
                                                             containee-bag :colour}]
                                                          [containee-bag container-bag quantity])
                                                        v))
                                                 dependency-map)
        ubergraph-container-to-containee-input (mapcat
                                                 (fn [[container-bag v]]
                                                   (map (fn [{quantity :quantity
                                                             containee-bag :colour}]
                                                          [container-bag containee-bag quantity])
                                                        v))
                                                 dependency-map)]
    {:containee-container (apply uber/digraph ubergraph-containee-to-container-input)
     :container-containee (apply uber/digraph ubergraph-container-to-containee-input)}))

(defn count-bags-able-to-hold
  [containee-bag input]
  (let [dependency-map         (reduce input->dependency-map {} input)
        containee-to-container (:containee-container (dependency-map->ubergraph dependency-map))]
    (- (count (alg/bf-traverse containee-to-container containee-bag))
       1)))

(defn traverse-counting
  ([from-node graph] (traverse-counting 0 from-node graph))
  ([acc from-node graph]
   (let [successors (uber/successors graph from-node)
         res        (if (empty? successors)
                      1
                      (->> successors
                           (map #(* (uber/weight graph from-node %)
                                    (traverse-counting % graph)))
                           (reduce +)
                           (+ 1)))]
     res)))

(defn count-bags-inside
  [container-bag input]
  (let [dependency-map         (reduce input->dependency-map {} input)
        container-to-containee (:container-containee (dependency-map->ubergraph dependency-map))]
    (- (traverse-counting container-bag container-to-containee)
       1)))

(defn -main
  [& args]
  (let [lines (clojure.string/split-lines (slurp *in*))]
    (println (count-bags-able-to-hold "shiny gold" lines))
    (println (count-bags-inside "shiny gold" lines))))
