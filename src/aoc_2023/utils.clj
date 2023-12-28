(ns aoc-2023.utils
  (:require [clojure.string :as str]))

(defn str-replace [match replacement string]
  (str/replace string match replacement))

(defn str-split [pattern string]
  (str/split string pattern))

(defn parse-int [string]
  (Integer/parseInt string))