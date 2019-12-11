(ns adventofcode.year2019.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn dir [[x y]]
  (let [deg (Math/toDegrees (Math/atan2 x (- y)))]
    (cond-> deg (neg? deg) (+ 360))))

(defn mag [v]
  (Math/sqrt (reduce + (map * v v))))

(defn los-vectors [grid]
  (let [all-cells (for [y (range (count grid)) x (range (count (grid y)))] [x y])
        occupied (filter (fn [[x y]] (= \# (get-in grid [y x]))) all-cells)]
    (into {}
          (for [cell occupied]
            [cell
             (->>
               (for [o occupied
                     :when (not= o cell)
                     :let [v (map - o cell)]]
                 [(dir v) (mag v) o])
               sort
               (group-by first)
               sort
               (map (fn [[k v]] [k (map rest v)])))]))))

(defn most-visible [lv]
  (->> lv
       (map (fn [[k v]] [k (count v)]))
       (apply max-key (fn [[_ v]] v))))

(defn part1 [grid]
  (most-visible (los-vectors grid)))

(defn skim [v]
  (mapv (fn [[_ [[_ v]]]] v) v))

(defn strip [v]
  (->> v
       (map (fn [[d v]] [d (seq (rest v))]))
       (filter second)))

(defn step [[a b]]
  [(into a (skim b)) (strip b)])

(defn part2 [grid]
  (let [l (los-vectors grid)
        best (->> l most-visible first l)]
    (->> [[] best]
         (iterate step)
         (remove (comp seq second))
         ffirst)))

(comment
  (los-vectors (->> (io/resource "adventofcode/year2019/day10/input0.txt") slurp cs/split-lines))
  (part2 (->> (io/resource "adventofcode/year2019/day10/input0.txt") slurp cs/split-lines))

  (= [[3 4] 8] (part1 (->> (io/resource "adventofcode/year2019/day10/input0.txt") slurp cs/split-lines)))
  (= [[5 8] 33] (part1 (->> (io/resource "adventofcode/year2019/day10/input1.txt") slurp cs/split-lines)))
  (= [[1 2] 35] (part1 (->> (io/resource "adventofcode/year2019/day10/input2.txt") slurp cs/split-lines)))
  (= [[6 3] 41] (part1 (->> (io/resource "adventofcode/year2019/day10/input3.txt") slurp cs/split-lines)))
  (= [[11 13] 210] (part1 (->> (io/resource "adventofcode/year2019/day10/input4.txt") slurp cs/split-lines)))
  (= [[23 20] 334] (part1 (->> (io/resource "adventofcode/year2019/day10/input5.txt") slurp cs/split-lines)))

  (let [res (part2 (->> (io/resource "adventofcode/year2019/day10/input4.txt") slurp cs/split-lines))]
    (and
      (= [11 12] (first res))
      (= [11 1] (last res))))

  (let [[x y] (nth (part2 (->> (io/resource "adventofcode/year2019/day10/input5.txt") slurp cs/split-lines)) 199)]
    (= 1119 (+ (* 100 x) y)))
  )