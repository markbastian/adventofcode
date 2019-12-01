(ns adventofcode.year2019.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn fuel-required [mass]
  (let [f (- (quot mass 3) 2)]
    (max f 0)))

(defn fuel-required-2 [mass]
  (reduce + (rest (take-while pos? (iterate fuel-required mass)))))

(comment
  (= 2 (fuel-required 12))
  (= 2 (fuel-required 14))
  (= 654 (fuel-required 1969))
  (= 33583 (fuel-required 100756))

  ;Part 1: 3262356
  (->> (io/resource "adventofcode/year2019/day01/input.txt")
       slurp
       cs/split-lines
       (map #(Long/parseLong %))
       (map fuel-required)
       (reduce +))

  ;Part 2: 4890664
  (->> (io/resource "adventofcode/year2019/day01/input.txt")
       slurp
       cs/split-lines
       (map #(Long/parseLong %))
       (map fuel-required-2)
       (reduce +))
  )
