(ns adventofcode.year2020.day02
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def test-input (->> (io/resource "adventofcode/year2020/day02/test-input.txt")
                     slurp
                     cs/split-lines))

(def input (->> (io/resource "adventofcode/year2020/day02/input.txt")
                slurp
                cs/split-lines))

(def rgx #"(\d+)-(\d+)\s+(\p{Alpha})\s*:\s*(\p{Alpha}+)")

(defn parse-line [pattern]
  (let [[_ lo hi [c] s] (re-matches rgx pattern)
        lo (Long/parseLong lo)
        hi (Long/parseLong hi)]
    [lo hi c (vec s)]))

(defn valid-password-part1? [pattern]
  (let [[lo hi c s] (parse-line pattern)
        freqs (frequencies s)
        freq (freqs c 0)]
    (<= lo freq hi)))

(defn valid-password-part2? [pattern]
  (let [[lo hi c s] (parse-line pattern)]
    (->> [lo hi] (map dec) (map s) (filter #{c}) count (= 1))))

(comment
  ;Part 1
  (time (= 2 (count (filter valid-password-part1? test-input))))
  (time (= 467 (count (filter valid-password-part1? input))))

  ;Part 2
  (time (= 1 (count (filter valid-password-part2? test-input))))
  (time (= 441 (count (filter valid-password-part2? input))))
  )