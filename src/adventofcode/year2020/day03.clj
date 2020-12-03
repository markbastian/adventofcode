(ns adventofcode.year2020.day03
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(defn to-input [input]
  (->> (io/resource input) slurp cs/split-lines (map cycle)))

(def test-input (to-input "adventofcode/year2020/day03/test-input.txt"))
(def input (to-input "adventofcode/year2020/day03/input.txt"))

(defn p [input [dy dx]]
  (loop [[f :as s] input pos 0 v []]
    (if (seq f)
      (recur (drop dy s) (+ pos dx) (conj v (first (drop pos f))))
      (count (filter #{\#} v)))))

(defn p2 [input slopes]
  (reduce * (map (partial p input) slopes)))

(comment
  (time (= 7 (p test-input [1 3])))
  (time (= 259 (p input [1 3])))

  (time (= 2 (p test-input [1 1])))
  (time (= 7 (p test-input [1 3])))
  (time (= 3 (p test-input [1 5])))
  (time (= 4 (p test-input [1 7])))
  (time (= 2 (p test-input [2 1])))

  (time (= 336 (part2 test-input [[1 1] [1 3] [1 5] [1 7] [2 1]])))
  (time (= 336 (p2 test-input [[1 1] [1 3] [1 5] [1 7] [2 1]])))
  (time (= 2224913600 (p2 input [[1 1] [1 3] [1 5] [1 7] [2 1]])))
  )