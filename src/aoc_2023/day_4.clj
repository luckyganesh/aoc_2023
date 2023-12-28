(ns aoc-2023.day-4
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math :as math]))

(def test_input (slurp "resources/test_inputs/day_4.txt"))
(def actual_input (slurp "resources/actual_inputs/day_4.txt"))
(defn str-replace [match replacement string]
  (str/replace string match replacement))

(defn str-split [pattern string]
  (str/split string pattern))

(defn parse-int [string]
  (Integer/parseInt string))

(def get-comb-ints (comp set (partial map parse-int) (partial str-split #" ")))

(defn get-input-data [string]
  (let [split-combinations (str-split #" \| " string)
        winning-combinations (get-comb-ints (first split-combinations))
        numbers (get-comb-ints (second split-combinations))]
    {:winning-combinations winning-combinations
     :numbers              numbers}))

(defn get-intersection [combinations]
  (set/intersection (:winning-combinations combinations) (:numbers combinations)))


(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map (partial str-replace #"  " " "))
       (map (partial str-split #": "))
       (map second)
       (map get-input-data)))

(defn get-winning-combos-count [input]
  (->> (parse-input input)
       (mapv (comp count get-intersection))))

(defn part_1 [input]
  (->> (get-winning-combos-count input)
       (filter (comp not zero?))
       (map (comp int (partial math/pow 2) dec))
       (apply +)))

(defn my-update-in [array index fun]
  (if (>= index (count array))
    array
    (update-in array [index] fun)))

(defn add-scratchcards [index till times init-scratchcards]
  (reduce #(my-update-in %1 %2 (partial + times)) init-scratchcards (range (inc index) (+ index till 1))))

(defn calculate_scratch_cards [winning_count]
  (let [number-of-cards (count winning_count)
        init-scratchcards (vec (repeat number-of-cards 1))]
    (loop [init-scratchcards init-scratchcards
           index 0]
      (if (>= index number-of-cards)
        init-scratchcards
        (let [index index
              till (get winning_count index)
              next-array (add-scratchcards index till (get init-scratchcards index) init-scratchcards)]
          (recur next-array (inc index)))))))

(def part_2 (comp (partial apply +)
                  calculate_scratch_cards
                  get-winning-combos-count))

(println (part_1 actual_input))
(println (part_2 actual_input))