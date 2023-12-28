(ns aoc-2023.day-2
  (:require [aoc-2023.utils :as utils]
            [clojure.string :as str]))



(def test_input (slurp "resources/test_inputs/day_2.txt"))
(def actual_input (slurp "resources/actual_inputs/day_2.txt"))

(def max-configuration {:blue 14, :red 12 :green 13})

(defn add-balls-into-map [m balls-color]
  (let [split-balls-color (str/split balls-color #" ")
        balls (Integer/parseInt (first split-balls-color))
        color (keyword (second split-balls-color))]
    (conj m {color balls})))

(defn get-set-details [string]
  (let [split-sets (str/split string #", ")
        sets (reduce add-balls-into-map {} split-sets)]
    sets))

(defn get-game-details [string]
  (let [game-sets (str/split string #": ")
        game-id (utils/parse-int (second (str/split (first game-sets) #" ")))
        sets (map get-set-details (str/split (second game-sets) #"; "))]
    {:id game-id :sets sets}))

(def parse-input (comp (partial map get-game-details) str/split-lines))

(defn is-color-matching [con1 con2 color]
  (<= (color con1) (color con2)))

(defn is-correct-set? [max-configuration actual-configuration]
  (let [test-keywords (keys actual-configuration)]
    (every? (partial is-color-matching actual-configuration max-configuration) test-keywords)))

(defn is-valid-game [game]
  (every? (partial is-correct-set? max-configuration) (:sets game)))

(def part_1 (comp (partial apply +)
                  (partial map :id)
                  (partial filter is-valid-game)
                  parse-input))

(def get-max-cubes-set (partial apply merge-with max))

(defn part_2 [input]
  (->> (parse-input input)
       (map (comp (partial apply *)
                  vals
                  get-max-cubes-set
                  :sets))
       (apply +)))

(println (part_1 actual_input))
(println (part_2 actual_input))