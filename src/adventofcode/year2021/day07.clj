(ns adventofcode.year2021.day07
  (:require [adventofcode.input-util :as iu]
            [clojure.java.math :as math]))

(def sample-input (iu/read-as-array "adventofcode/year2021/day07/sample-input.txt"))
(def input (iu/read-as-array "adventofcode/year2021/day07/input.txt"))

(defn total-cost [input m v]
  (reduce + (map (fn [i] (m (math/abs ^long (- i v)))) input)))

(defn brute-force [input cost-fn]
  (let [min (apply min input)
        max (apply max input)]
    (->> (range min (inc max))
         (map (fn [i] [i (total-cost input cost-fn i)]))
         (apply min-key second))))

(defn costs [input]
  (->> (apply max input)
       range
       (map inc)
       (reductions (fn [v i] (+ v i)) 0)
       vec))

(comment
  ;;[step answer]
  (= [2 37] (brute-force sample-input identity))
  (= [371 341558] (brute-force input identity))

  (= [5 168] (brute-force sample-input (costs sample-input)))
  (= [484 93214037] (brute-force input (costs input)))
  )
