(ns aoc-2023.day-5
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]))


(def test_input (slurp "resources/test_inputs/day_5.txt"))
(def actual_input (slurp "resources/actual_inputs/day_5.txt"))

(def destination-mapping-names [:seed-to-soil
                                :soil-to-fertilizer
                                :fertilizer-to-water
                                :water-to-light
                                :light-to-temperature
                                :temperature-to-humidity
                                :humidity-to-location])

(defn add-mapping-numbers [init-mappings string]
  (let [mapping-split (utils/str-split #":\n" string)
        mapping-name (keyword (first (utils/str-split #" " (first mapping-split))))
        mappings (mapv (partial utils/str-split-long #" ")
                       (str/split-lines (second mapping-split)))]
    (conj init-mappings {mapping-name mappings})))
(defn parse-input [input]
  (let [split-mappings (utils/str-split #"\n\n" input)
        seeds (utils/str-split-long #" " (second (utils/str-split #": " (first split-mappings))))
        mappings (reduce add-mapping-numbers {} (rest split-mappings))]
    {:seeds    seeds
     :mappings mappings}))

(defn is-in-boundaries [source [destination-number source-number till]]
  (and (>= source source-number) (< source (+ source-number till))))

(defn calculate-destination [source [destination-number source-number till]]
  (+ destination-number (- source source-number)))

(defn get-destination [mapping source-number]
  (->> (conj mapping [source-number source-number 1])
       (filter (partial is-in-boundaries source-number))
       first
       (calculate-destination source-number)))

(defn map-to-destination [mappings source-numbers mapping-name]
  (map (partial get-destination (mapping-name mappings)) source-numbers))

(defn sol [mappings source-numbers]
  (apply min (reduce (partial map-to-destination mappings) source-numbers destination-mapping-names)))

(defn part-1 [input]
  (let [data (parse-input input)] (sol (:mappings data) (:seeds data))))

(println (part-1 actual_input))

(defn get-range [start till]
  (range start (+ start till)))

(defn part-2 [input]
  (let [data (parse-input input)]
    (->> (partition 2 (:seeds data))
         (map (partial apply get-range))
         (map (partial sol (:mappings data)))
         (apply min))))

(println (part-2 actual_input))