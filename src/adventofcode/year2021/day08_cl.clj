(ns adventofcode.year2021.day08-cl
  (:refer-clojure :exclude [==])
  (:require [adventofcode.input-util :as iu]
            [clojure.string :as str]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd])
  (:import (clojure.lang PersistentQueue)))

(defn parse-line [line]
  (let [[a b] (str/split line #"\s*\|\s+")
        input (map set (str/split a #"\s+"))
        output (map set (str/split b #"\s+"))]
    [(vec (sort-by count input)) output]))

(def test-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(parse-line test-line)

(def sample-input (mapv parse-line (iu/read-as-strings "adventofcode/year2021/day08/sample-input.txt")))
(def input (mapv parse-line (iu/read-as-strings "adventofcode/year2021/day08/input.txt")))

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

(defn setup [[input output]]
  (let [{[two] 2 [three] 3 [four] 4 five 5 six 6 [seven] 7} (group-by count input)]
    {:locked  {1 two
               4 four
               7 three
               8 seven}
     :unknown {0 (set six)
               2 (set five)
               3 (set five)
               5 (set five)
               6 (set six)
               9 (set six)}}))










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

(sort-by
  (fn [[i s]] (count s))
  {0 "abcefg"
   1 "cf"
   2 "acdeg"
   3 "acdfg"
   4 "bcdf"
   5 "abdfg"
   6 "abdefg"
   7 "acf"
   8 "abcdefg"
   9 "abcdfg"})

(def char->digit
  (reduce
    (fn [m [s i]]
      (update m s (comp set conj) i))
    {}
    (for [[i st] digit->char s st] [s i])))


(comment
  ;;[step answer]
  (= [2 37] (brute-force sample-input identity))
  (= [371 341558] (brute-force input identity))

  (= [5 168] (brute-force sample-input (costs sample-input)))
  (= [484 93214037] (brute-force input (costs input)))
  )

(into
  (sorted-map)
  (update-vals digit->char (comp set sort #(map (comp symbol str) %))))

(let [[a b] (parse-line test-line)]
  (into (sorted-map)
        (zipmap '[n0 n1 n2 n3 n4 n5 n6 n7 n8 n9] (sort-by count (map (comp set sort) a)))))

(run* [n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 a b c d e f g]
      (== {n0 #{\a \b},
           n1 #{\a \b \d},
           n2 #{\a \b \e \f},
           n3 #{\b \c \d \e \f},
           n4 #{\a \c \d \f \g},
           n5 #{\a \b \c \d \f},
           n6 #{\a \b \c \d \e \f},
           n7 #{\b \c \d \e \f \g},
           n8 #{\a \b \c \d \e \g},
           n9 #{\a \b \c \d \e \f \g}}
          {0 #{a e c g b f},
           1 #{c f},
           2 #{a e c g d},
           3 #{a c g d f},
           4 #{c b d f},
           5 #{a g b d f},
           6 #{a e g b d f},
           7 #{a c f},
           8 #{a e c g b d f},
           9 #{a c g b d f}}))

(run* [k]
      (fresh [a b]
             (== {k #{\a \b}}
                 {1 #{a b}})))

(run* [k v a b]
      (membero v [\a \b])
      (membero v [a b])
      (== {k v} {1 v}))

(run* [k]
      (== {k 2} {1 2}))


(run* [n9 c f]
      (== [n9 [\a \b]]
          [1 [c f]]))


(run* [q]
      (fresh [x y]
             (== {x 1} {x y})
             (== {:x y} {x y})
             (== q {x y})))

(run* [q] (fresh [x y]
                 (== [x 2] [1 y])
                 (== q [x y])))

(run* [a b]
      (== {:a (membero a [1 2 3])
           :b 2}
          {:a [\a \b]
           :b b}))

;(run* [q]
;      (== q true))
;
;(let [vars [a b c d e f g]]
;  ())
;
(run* [q]
      (membero q [1 2 3])
      (membero q [2 3 4]))