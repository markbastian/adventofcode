(ns adventofcode.year2019.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def input1 (slurp (io/resource "adventofcode/year2019/day06/input1.txt")))
(def input2 (slurp (io/resource "adventofcode/year2019/day06/input2.txt")))
(def input (slurp (io/resource "adventofcode/year2019/day06/input.txt")))

(defn orbit-path [m s]
  (take-while identity (iterate m s)))

(defn orbit-count [m s]
  (count (rest (orbit-path m s))))

(defn orbits [input]
  (->> input
       cs/split-lines
       (map #(cs/split % #"\)"))
       (reduce (fn [m [src dst]] (assoc m dst src)) {})))

(defn part1 [input]
  (let [o (orbits input)
        nodes (distinct (into (keys o) (vals o)))]
    (reduce + (map (partial orbit-count o) nodes))))

(defn part2 [input]
  (let [o (orbits input)
        path1 (orbit-path o "YOU")
        path2 (orbit-path o "SAN")
        tx-in (dec (count (remove (set path2) path1)))
        tx-out (dec (count (remove (set path1) path2)))]
    (+ tx-in tx-out)))

(comment
  (= 42 (part1 input1))
  (= 453028 (part1 input))

  (= 4 (part2 input2))
  (= 562 (part2 input))
  )