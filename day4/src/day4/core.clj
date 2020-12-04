(ns day4.core
  (:gen-class))

(def required-fields [:byr :iyr :eyr :hgt :hcl :ecl :pid])

(defn parse-int! [s] (try (Integer/valueOf s) (catch Exception e nil))) ; This would ideally not swallow the exception
(defn number-between [lower-bound upper-bound] #(<= lower-bound (parse-int! %) upper-bound))
(def birth-year-validator (number-between 1920 2002))
(def issue-year-validator (number-between 2010 2020))
(def expiration-year-validator (number-between 2020 2030))

(def height-validator-by-unit {"cm" (number-between 150 193)
                               "in" (number-between 59 76)})


(def field-validators {:byr {:required      true
                             :validation-fn birth-year-validator}
                       :iyr {:required      true
                             :validation-fn issue-year-validator}
                       :eyr {:required      true
                             :validation-fn expiration-year-validator}
                       :hgt {:required      true
                             :validation-fn #(let [[_ value-as-string unit] (re-matches #"(\d{2,3})(cm|in)" %)
                                                   value                    (parse-int! value-as-string)
                                                   validator-fn             (get height-validator-by-unit unit (constantly false))]
                                               (boolean (when value (validator-fn value))))}
                       :hcl {:required      true
                             :validation-fn #(some? (re-matches #"#[a-f0-9]{6}" %))}
                       :ecl {:required      true
                             :validation-fn #(some? (some #{%} #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}))}
                       :pid {:required      true
                             :validation-fn #(some? (re-matches #"[0-9]{9}" %))}
                       :cid {:required      false
                             :validation-fn (constantly true)}})

(defn group-entries
  [input]
  (map #(clojure.string/replace % #"\n" " ")
       (clojure.string/split input #"\n\n")))

(defn entry->passport
  [entry]
  (->> (re-seq #"([a-z]+):([a-z0-9#]+)( |$)" entry)
       (map (fn [[_ key-as-string val]] [(keyword key-as-string) val]))
       (into {})))

(defn passport-contains-required-fields?
  [passport]
  (every? passport required-fields))

(defn passport-required-fields-are-valid?
  [passport]
  (->> field-validators
       seq
       (every? (fn [[field {:keys [required validation-fn]}]]
                 (if-let [passport-field (field passport)]
                   (validation-fn passport-field)
                   (not required))))))

(defn count-valid-passports
  [input validation-fn]
  (->> input
       group-entries
       (map entry->passport)
       (filter validation-fn)
       count))

(defn -main
  [& args]
  (let [input (slurp *in*)]
    (println (count-valid-passports input passport-contains-required-fields?))
    (println (count-valid-passports input passport-required-fields-are-valid?))))
