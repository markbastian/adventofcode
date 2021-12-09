(ns adventofcode.year2021.day08
  (:require [adventofcode.input-util :as iu]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo])
  (:import (clojure.lang PersistentQueue)))

(defn parse-line [line]
  (let [[a b] (str/split line #"\s*\|\s+")
        input (map set (str/split a #"\s+"))
        output (map set (str/split b #"\s+"))]
    [(set (sort-by count input)) output]))

(def test-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(def test-input (parse-line test-line))

(def sample-input (mapv parse-line (iu/read-as-strings "adventofcode/year2021/day08/sample-input.txt")))
(def input (mapv parse-line (iu/read-as-strings "adventofcode/year2021/day08/input.txt")))

(def mappings
  (->> (combo/permutations "abcdefg")
       (map (partial zipmap "abcdefg"))))

(def digit->char
  (update-vals
    {0 "abcefg"
     1 "cf"
     2 "acdeg"
     3 "acdfg"
     4 "bcdf"
     5 "abdfg"
     6 "abdefg"
     7 "acf"
     8 "abcdefg"
     9 "abcdfg"}
    set))

(def decoder (zipmap (vals digit->char) (keys digit->char)))

(def target (set (vals digit->char)))

(defn part1 [input]
  (let [x (update-vals digit->char count)
        f (frequencies (vals x))
        unique-counts (reduce (fn [s [i ct]] (cond-> s (= 1 ct) (conj i))) #{} f)]
    (->> input
         (map (fn [[_ o]] (filter unique-counts (map count o))))
         flatten
         count)))

(defn rewire [[i o] m]
  [(set (map (fn [x] (set (map m x))) i))
   (map (fn [x] (set (map m x))) o)])

(defn rewire-line [line]
  (some
    (fn [m]
      (let [[ni no] (rewire line m)]
        (when (= target ni)
          {:mapping m
           :rhs     (parse-long (str/join (map str (map decoder no))))})))
    mappings))

(defn part2 [input]
  (->> input
       (map (comp :rhs rewire-line))
       (reduce +)))

(comment
  (= 26 (part1 sample-input))
  (= 539 (part1 input))
  (= 61229 (part2 sample-input))
  (= 1084606 (part2 input))
  )

;; Future work ;;
(def queue PersistentQueue/EMPTY)

{:from    (set "abcdefg")
 :to      (set "abcdefg")
 :mapping {}
 :x       (partition-by count (sort-by count target))}