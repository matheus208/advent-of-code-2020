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
  ([instructions] (fix-and-execute 0 instructions {:acc 0, :lines-visited [], :fixed-instruction nil}))
  ([line instructions context]
   (assert (<= 0 line)
           (str "Line must be between 0 and " (count instructions)". But it was " line))
   (if (some #{line} (:lines-visited context))
     (throw (ex-info "We should not be executing this line twice" {:current-line line, :context context}))
     (if (>= line (count instructions))
       context
       (let [[instruction argument] (nth instructions line)
             attempt-fix?           (nil? (:instruction-fixed context))
             new-context            (update context :lines-visited conj line)]
        (case instruction
          :acc (recur (inc line) instructions (update new-context :acc + argument))
          :nop (if (and attempt-fix?
                        (terminates? (+ line argument) instructions new-context)) ;if terminates by considering it as a jump
                 (recur (+ line argument) instructions (assoc new-context :fixed-instruction line)) ; consider it as a jump
                 (recur (inc line) instructions new-context)) ; continue as nop
          :jmp (if (and attempt-fix?
                        (terminates? (inc line) instructions new-context)) ; if terminates by considering it as nop
                 (recur (inc line) instructions (assoc new-context :fixed-instruction line)) ; consider it as nop
                 (recur (+ line argument) instructions new-context)) ; continue as jmp
          (throw (ex-info "Instruction not supported" {:instruction instruction}))))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [instructions           (input->instructions (slurp *in*))
        execute-result         (execute instructions)
        fix-and-execute-result (fix-and-execute-2 (mapv identity instructions))]
    (println execute-result)
    (println fix-and-execute-result)))
