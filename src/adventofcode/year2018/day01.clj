(ns adventofcode.year2018.day01
  (:require [clojure.string :as cs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))


"+1, +1, +1 results in  3\n+1, +1, -2 results in  0\n-1, -2, -3 results in -6"

(defn freq-adjuster [s]
  (->> s
       (re-seq #"[+-]\d+")
       (map (comp edn/read-string #(cs/replace % "+" "")))
       (reduce +)))

(comment
  ;510
  (-> "adventofcode/year2018/day01/day0101.txt" io/resource slurp freq-adjuster))

(defn freq-x2 [s]
  (->> s
       (re-seq #"[+-]\d+")
       (map (comp edn/read-string #(cs/replace % "+" "")))
       cycle
       (reductions +)
       (reduce (fn [v n] (if (v n) (reduced n) (conj v n))) #{})))

(comment
  ;10
  (freq-x2 "+3, +3, +4, -2, -4")
  ;69074
  (-> "adventofcode/year2018/day01/day0101.txt" io/resource slurp freq-x2))