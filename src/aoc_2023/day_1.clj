(ns aoc_2023.day_1
  (:require [clojure.string :as str]))

(def test_input (slurp "resources/test_inputs/day_1.txt"))
(def test_input_2 (slurp "resources/test_inputs/day_1_test_2.txt"))
(def actual_input (slurp "resources/actual_inputs/day_1.txt"))

(def string-symbols {"one"   1
                     "two"   2
                     "three" 3
                     "four"  4
                     "five"  5
                     "six"   6
                     "seven" 7
                     "eight" 8
                     "nine"  9
                     "zero"  0})

(def digit-symbols {"1" 1
                    "2" 2
                    "3" 3
                    "4" 4
                    "5" 5
                    "6" 6
                    "7" 7
                    "8" 8
                    "9" 9
                    "0" 0})

(defn filter-symbols-in-string [symbols string]
  (loop [string string
         filtered-strings []]
    (if (empty? string)
      filtered-strings
      (let [filtered-symbols? (filter (partial str/starts-with? string) symbols)
            filtered-strings (if (empty? filtered-symbols?)
                                  filtered-strings
                                  (conj filtered-strings (first filtered-symbols?)))]
        (recur (subs string 1) filtered-strings)))))

(defn get-first-last-digits [symbols string]
  (->> (filter-symbols-in-string (keys symbols) string )
       ((juxt first last))
       (map (partial get symbols))
       (apply str)
       Integer/parseInt))

(defn solution [symbols input]
  (->> (str/split-lines input)
       (map (partial get-first-last-digits symbols))
       (apply +)))

(def part_1 (partial solution digit-symbols))
(def part_2 (partial solution (merge digit-symbols string-symbols)))

(println (part_1 actual_input))
(println (part_2 actual_input))