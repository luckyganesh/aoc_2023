(ns aoc-2023.day-6
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]))

(def test_input (slurp "resources/test_inputs/day_6.txt"))
(def actual_input (slurp "resources/actual_inputs/day_6.txt"))

(defn parse-input [input]
  (let [split-line (str/split-lines input)
        times (utils/str-split-int #" " (str/replace (second (str/split (first split-line) #": *")) #"  *" " "))
        distances (utils/str-split-int #" " (str/replace (second (str/split (second split-line) #": *")) #"  *" " "))]
    {:times times :distances distances}))

(defn count-the-win-possibilities [time distance]
  (count (filter (partial < distance) (map #(* (- time %) %) (range 1 time)))))

(defn part-1 [input]
  (let [data (parse-input input)]
    (apply * (map count-the-win-possibilities (:times data) (:distances data)))))

(defn parse-input-2 [input]
  (let [split-line (str/split-lines input)
        times (parse-long (str/replace (second (str/split (first split-line) #": *")) #" " ""))
        distances (parse-long (str/replace (second (str/split (second split-line) #": *")) #" " ""))]
    {:times times :distances distances}))
(println (part-1 actual_input))

(defn part-2 [input]
  (let [data (parse-input-2 input)]
    (count-the-win-possibilities (:times data) (:distances data))))

(println (part-2 actual_input))

