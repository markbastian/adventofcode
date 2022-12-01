(ns adventofcode.year2021.day18
  (:require [adventofcode.input-util :as iu]
            [clojure.edn :as edn]))

(def sample-input (->> (iu/read-as-strings "adventofcode/year2021/day18/sample-input.txt")
                       (mapv edn/read-string)))

(def input (->> (iu/read-as-strings "adventofcode/year2021/day18/input.txt")
                (mapv edn/read-string)))

(let [[[[[[a b] c] d] e] f] [[[[[9, 8], 1], 2], 3], 4]]
  [[[[0 (+ b c)] d] e] f])

(let [[a [b [c [d [e f]]]]] [7 [6 [5 [4 [3 2]]]]]]
  [a [b [c [(+ d e) 0]]]])

(let [[a [b [c [d [e f]]]]] [3,[2,[1,[7,3]]]]]
  [a [b [c [(+ d e) 0]]]])

[[3,[2,[1,[7,3]]]]
 [6,[5,[4,[3,2]]]]]