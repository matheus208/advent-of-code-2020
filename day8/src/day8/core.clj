(ns day8.core
  (:gen-class)
  (:require [clojure.string]))

(def test-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn input->instructions
  [input]
  (->> input
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))
       (map (fn [[instruction arg]] [(keyword instruction) (Integer/valueOf arg)]))))

(defn execute
  ([instructions] (execute 0 instructions {:acc 0, :lines-visited []}))
  ([line instructions context]
   (assert (<= 0 line)
           (str "Line must be between 0 and " (count instructions)". But it was " line))
   (if (or (some #{line} (:lines-visited context))
           (>= line (count instructions)))
     context
     (let [[instruction argument] (nth instructions line)
           new-context            (update context :lines-visited conj line)]
       (case instruction
         :acc (recur (inc line) instructions (update new-context :acc + argument))
         :nop (recur (inc line) instructions new-context)
         :jmp (recur (+ line argument) instructions new-context)
         (throw (ex-info "Instruction not supported" {:instruction instruction})))))))

(defn terminates?
  ([instructions] (terminates? 0 instructions {:acc 0, :lines-visited []}))
  ([line instructions context]
   (assert (<= 0 line)
           (str "Line must be between 0 and " (count instructions)". But it was " line))
   (cond
     (some #{line} (:lines-visited context))
     false

     (> line (count instructions))
     false

     (= line (count instructions))
     true

     :else
     (let [[instruction argument] (nth instructions line)
           new-context            (update context :lines-visited conj line)]
       (case instruction
         :acc (recur (inc line) instructions (update new-context :acc + argument))
         :nop (recur (inc line) instructions new-context)
         :jmp (recur (+ line argument) instructions new-context)
         (throw (ex-info "Instruction not supported" {:instruction instruction})))))))

(defn fix-and-execute
  [instructions]
  (if-let [context (terminates? instructions)]
    context                                     ;we didn't need to replace anything
    (reduce-kv
     (fn [_ index [instruction argument]]
       (let [new-instruction-set  (case instruction
                                    :nop (assoc instructions index [:jmp argument])
                                    :jmp (assoc instructions index [:nop argument])
                                    instructions)]
         (when (terminates? new-instruction-set)
           (reduced (assoc (execute new-instruction-set) :fixed-instruction index)))))
     nil
     instructions)))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [instructions           (input->instructions (slurp *in*))
        execute-result         (execute instructions)
        fix-and-execute-result (fix-and-execute (mapv identity instructions))]
    (println execute-result)
    (println fix-and-execute-result)))
