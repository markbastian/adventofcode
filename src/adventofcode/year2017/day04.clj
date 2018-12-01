(ns adventofcode.year2017.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def passphrases
  (->> "adventofcode/year2017/day04/input.txt"
       io/resource
       slurp
       cs/split-lines
       (map #(cs/split % #"\s+"))))

;386
(->> passphrases
     (filter #(->> % frequencies vals (every? #{1})))
     count)

;208
(->> passphrases
     (filter #(->> % (map frequencies) frequencies vals (every? #{1})))
     count)