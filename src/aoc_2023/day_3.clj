(ns aoc-2023.day-3
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [aoc-2023.utils :as utils]))

(def test_input (slurp "resources/test_inputs/day_3.txt"))
(def actual_input (slurp "resources/actual_inputs/day_3.txt"))

(defn parse-input [input]
  (mapv (partial utils/str-split #"") (str/split-lines input)))

(defn is-in-between [minimum maximum number]
  (and (<= minimum number) (<= number maximum)))

(defn is-coordinate-in-between [[min-x min-y] [max-x max-y] [x y]]
  (and (is-in-between min-x max-x x) (is-in-between min-y max-y y)))

(def digit-str #{"1" "2" "3" "4" "5" "6" "7" "8" "9" "0"})

(def not-symbols #{"."})

(defn add-number-coordinates [numbers-coordinates number-str number-coordinates]
  (conj numbers-coordinates {:number (utils/parse-int number-str) :co-ordinates number-coordinates}))

(defn get-next-coordinate [[max-x max-y] [x y]]
  (if (< x max-x) [(inc x) y] [0 (inc y)]))

(defn get-number-coordinates [board]
  (let [board-final-x (dec (count (first board)))
        board-final-y (dec (count board))]
    (loop [[x y] [0 0]
           numbers-coordinates []
           is-number-started? false
           number-str ""
           number-started-coordinates []]
      (if (is-coordinate-in-between [0 0] [board-final-x board-final-y] [x y])
        (let [string (get-in board [y x])
              is-digit? (digit-str string)
              next-coordinate (get-next-coordinate [board-final-x board-final-y] [x y])
              is-on-edge? (= x board-final-x)]
          (cond
            (and (not is-number-started?) (not is-digit?)) (recur next-coordinate numbers-coordinates false "" [])
            (and (not is-number-started?) is-digit? (not is-on-edge?)) (recur next-coordinate numbers-coordinates true string [x y])
            (and (not is-number-started?) is-digit? is-on-edge?) (recur next-coordinate (add-number-coordinates numbers-coordinates string [[x y] [x y]]) false "" [])
            (and is-number-started? is-digit? (not is-on-edge?)) (recur next-coordinate numbers-coordinates true (str/join [number-str string]) number-started-coordinates)
            (and is-number-started? is-digit? is-on-edge?) (recur next-coordinate (add-number-coordinates numbers-coordinates (str/join [number-str string]) [number-started-coordinates [x y]]) false "" [])
            :else (recur next-coordinate (add-number-coordinates numbers-coordinates number-str [number-started-coordinates [(dec x) y]]) false "" [])))
        numbers-coordinates))))

(defn get-neighbour-coordinates [[[board-init-x board-init-y] [board-last-x board-last-y]]
                                 [[number-init-x number-init-y] [number-last-x number-last-y]]]
  (for [x (range (dec number-init-x) (+ 2 number-last-x))
        y (range (dec number-init-y) (+ 2 number-last-y))
        :when (and (not (is-coordinate-in-between [number-init-x number-init-y] [number-last-x number-last-y] [x y]))
                   (is-coordinate-in-between [board-init-x board-init-y] [board-last-x board-last-y] [x y]))]
    [x y]))

(defn is-part-number? [board number-coordinates]
  (let [board-last-x (dec (count (first board)))
        board-last-y (dec (count board))
        neighbour-coordinates (get-neighbour-coordinates [[0 0] [board-last-x board-last-y]] number-coordinates)]
    (not-every? (comp not-symbols) (map (comp (partial get-in board) reverse) neighbour-coordinates))))

(defn is-part-number-coordinates [board number-data]
  (is-part-number? board (:co-ordinates number-data)))

(defn part-1 [input]
  (let [board (parse-input input)]
    (->> board
         get-number-coordinates
         (filter (partial is-part-number-coordinates board))
         (map :number)
         (apply +))))

(defn add-gear-data [board number-data]
  (let [board-last-x (dec (count (first board)))
        board-last-y (dec (count board))
        neighbour-coordinates (get-neighbour-coordinates [[0 0] [board-last-x board-last-y]] (:co-ordinates number-data))
        gear-coordinates (filter (comp (partial = "*") (partial get-in board) reverse) neighbour-coordinates)]
    (if (empty? gear-coordinates)
      (conj number-data {:is-gear-number? false :gear-coordinate []})
      (conj number-data {:is-gear-number? true :gear-coordinate gear-coordinates}))))

(defn part-2 [input]
  (let [board (parse-input input)]
    (->> board
         get-number-coordinates
         (map (partial add-gear-data board))
         (filter :is-gear-number?)
         (group-by :gear-coordinate)
         (filter (comp (partial not= 1) count second))
         (map second)
         (map (partial map :number))
         (map (partial apply *))
         (apply +))))

(println (part-1 actual_input))
(println (part-2 actual_input))