(ns adventofcode.year2021.day01
  (:require [adventofcode.input-util :as iu]))

(def sample-input [199 200 208 210 200 207 240 269 260 263])
(defonce input (iu/read-as-array "adventofcode/year2021/day01/input.txt"))

(defn n-larger-measurements [input]
  (->> (map (fn [a b] (< a b)) input (rest input))
       (filter identity)
       count))

(defn averaged-larger-measurements [input]
  (->> input
       (partition 3 1)
       (map #(apply + %))
       n-larger-measurements))

(comment
  ;; Part 1
  (= 7 (n-larger-measurements sample-input))
  (= 1553 (n-larger-measurements input))

  ;; Part 2
  (= 5 (averaged-larger-measurements sample-input))
  (= 1597 (averaged-larger-measurements input))
  )