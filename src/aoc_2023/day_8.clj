(ns aoc-2023.day-8
  (:require [aoc-2023.utils :as utils]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def test_input (slurp "resources/test_inputs/day_8.txt"))
(def test_input_2 (slurp "resources/test_inputs/day_8_test_2.txt"))
(def test_input_3 (slurp "resources/test_inputs/day_8_test_3.txt"))
(def actual_input (slurp "resources/actual_inputs/day_8.txt"))

(defn add-node [nodes string]
  (let [split-string (utils/str-split #" = " string)
        node-name (keyword (first split-string))
        directions (map keyword (utils/str-split #" " (str/replace (second split-string) #"\(|\)|," "")))]
    (conj nodes {node-name {:R (second directions) :L (first directions)}})))

(defn parse-input [input]
  (let [string (utils/str-split #"\n\n" input)
        path (mapv keyword (utils/str-split #"" (first string)))
        nodes (reduce add-node {} (str/split-lines (second string)))]
    [path nodes]))


(defn ends-with? [subs s]
  (str/ends-with? s subs))

(defn cal-steps [nodes path match-condition-fn node]
  (let [no-of-paths (count path)]
    (loop [n 0 node node]
      (let [direction (get path (rem n no-of-paths))
            next-node (direction (node nodes))]
        (if (match-condition-fn next-node)
          (inc n)
          (recur (inc n) next-node))))))

(defn part-1 [input]
  (let [[path nodes] (parse-input input)]
    (cal-steps nodes path (partial = :ZZZ) :AAA)))

(defn part-2 [input]
  (let [[path nodes] (parse-input input)]
    (->> (filter (partial ends-with? "A") (keys nodes))
         (map (partial cal-steps nodes path (partial ends-with? "Z")))
         (reduce math/lcm))))

(println (part-1 actual_input))
(println (part-2 actual_input))