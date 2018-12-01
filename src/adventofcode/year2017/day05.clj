(ns adventofcode.year2017.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.edn :as edn]))

(def jumps
  (->> "adventofcode/year2017/day05/input.txt"
       io/resource
       slurp
       cs/split-lines
       (mapv (comp edn/read-string cs/trim))))

(defn step [[offsets v]]
  (let [n (+ v (offsets v))]
    [(update offsets v inc) n]))

(defn jump-steps [jumps]
  (count
    (take-while
      (fn [[s v]] (contains? s v))
      (iterate step [jumps 0]))))

;354121
;(jump-steps jumps)

(defn step2 [[offsets v]]
  (let [n (+ v (offsets v))]
    [(update offsets v #((if (>= % 3) dec inc) %)) n]))

(defn jump-steps2 [jumps]
  (count
    (take-while
      (fn [[s v]] (contains? s v))
      (iterate step2 [jumps 0]))))

(comment
  (jump-steps2 [0 3 0 1 -3])
  ;27283023 - takes a while
  (jump-steps2 jumps))

