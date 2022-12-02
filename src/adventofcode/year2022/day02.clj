(ns adventofcode.year2022.day02
  (:require [adventofcode.input-util :as iu]
            [clojure.string :as str]))

(def sample-input
  (->> (iu/read-as-strings "adventofcode/year2022/day02/sample-input.txt")
       (map #(map keyword (str/split % #"\s+")))))

(def input
  (->> (iu/read-as-strings "adventofcode/year2022/day02/input.txt")
       (map #(map keyword (str/split % #"\s+")))))

(defn score [args]
  (case args
    ([:rock :paper] [:paper :scissors] [:scissors :rock]) 6
    ([:paper :rock] [:scissors :paper] [:rock :scissors]) 0
    ([:rock :rock] [:scissors :scissors] [:paper :paper]) 3))

(defn resolve-game1 [input]
  (let [[_ shape :as args] (mapv {:A :rock :B :paper :C :scissors
                                  :X :rock :Y :paper :Z :scissors} input)
        outcome (score args)
        shape-value ({:rock 1 :paper 2 :scissors 3} shape)]
    (+ shape-value outcome)))

(defn step1 [input]
  (->> input
       (map resolve-game1)
       (reduce +)))

(comment
  (= 15 (step1 sample-input))
  (= 9241 (step1 input)))

(defn resolve-game2 [input]
  (let [[that-shape :as strategy] (mapv {:A :rock :B :paper :C :scissors
                                         :X :lose :Y :draw :Z :win} input)
        shape (case strategy
                [:rock :win] :paper
                [:paper :win] :scissors
                [:scissors :win] :rock
                [:paper :lose] :rock
                [:scissors :lose] :paper
                [:rock :lose] :scissors
                [:rock :draw] :rock
                [:scissors :draw] :scissors
                [:paper :draw] :paper)
        outcome (score [that-shape shape])
        shape-value ({:rock 1 :paper 2 :scissors 3} shape)]
    (+ shape-value outcome)))

(defn step2 [input]
  (->> input
       (map resolve-game2)
       (reduce +)))

(comment
  (= 12 (step2 sample-input))
  (= 14610 (step2 input)))
