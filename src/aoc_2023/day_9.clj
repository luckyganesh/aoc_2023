(ns aoc-2023.day-9
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]))

(def test_input (slurp "resources/test_inputs/day_9.txt"))
(def actual_input (slurp "resources/actual_inputs/day_9.txt"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map (partial utils/str-split-long #" "))))

(defn find-next-value-of-history [history]
  (loop [history (reverse history)
         path []]
    (if (every? zero? history)
      (reduce + path)
      (recur (map (partial apply -) (partition 2 1 history)) (conj path (first history))))))


(defn reverse-subtract [num1 num2]
  (- num2 num1))

(defn find-previous-value-of-history [history]
  (loop [history (reverse history)
         path []]
    (if (every? zero? history)
      (reduce reverse-subtract (reverse path))
      (recur (map (partial apply -) (partition 2 1 history)) (conj path (last history))))))

(def part-1 (comp (partial apply +) (partial map find-next-value-of-history) parse-input))
(def part-2 (comp (partial apply +) (partial map find-previous-value-of-history) parse-input))

(println (part-1 actual_input))
(println (part-2 actual_input))