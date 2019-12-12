(ns adventofcode.year2019.day06cl
  (:require [clojure.java.io :as io]
            [clojure.core.logic :refer [run* == membero conjo conde conso firsto resto fresh llist]]))

(def input1 (slurp (io/resource "adventofcode/year2019/day06/input1.txt")))
(def input2 (slurp (io/resource "adventofcode/year2019/day06/input2.txt")))
(def input (slurp (io/resource "adventofcode/year2019/day06/input.txt")))

;https://github.com/clojure/core.logic/wiki/A-Core.logic-Primer

(run* [q]
      (== q true))

(run* [q]
      (membero q [1 2 3])
      (membero q [2 3 4]))

(run* [q]
      (conde
        [(membero q [1 2 3 4 5 6 7 8])
         (conso q [5 6] [4 5 6])]))

(run* [q]
      (resto [1 2 3 4] q))

(run* [q]
      (firsto [1 2 3 4] q))


(run* [q s]
      (fresh [pr]
             (membero q (range 10))
             (membero pr [[1 2] [2 3] [3 4]])
             (firsto s 3)
             (firsto pr q)
             (conso q s pr)))

(run* [q s]
      (fresh [a b pr]
             (membero q (range 10))
             (membero pr [[1 2] [2 3] [3 4]])
             (firsto s 1)
             (conso a (llist s 5) q)))