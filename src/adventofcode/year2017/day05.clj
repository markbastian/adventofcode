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
  [(update offsets v inc) (+ v (offsets v))])

(defn jump-steps [step-fn jumps]
  (->> (iterate step-fn [jumps 0])
       (take-while (fn [[s v]] (contains? s v)))
       count))

(comment
  (step (step (step [jumps 0])))
  (take 20 (map second (iterate step [jumps 0])))
  ;354121
  (jump-steps step jumps))

(defn step2 [[offsets v]]
  [(update offsets v (fn [i] ((if (>= i 3) dec inc) i)))
   (+ v (offsets v))])

(comment
  ;10
  (jump-steps step2 [0 3 0 1 -3])
  ;27283023 - takes a while
  (jump-steps step2 jumps))

