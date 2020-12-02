(ns adventofcode.year2020.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.math.combinatorics :as combo]))

(defonce test-input [1721 979 366 299 675 1456])

(defonce input (->> (io/resource "adventofcode/year2020/day01/input.txt")
                    slurp
                    cs/split-lines
                    (map #(Long/parseLong %))))

(defn find-entries [n input]
  (->> (combo/combinations input n)
       (some (fn [v] (when (= 2020 (apply + v)) v)))
       (apply *)))

(comment
  (time (= 514579 (find-entries 2 test-input)))
  (time (= 138379 (find-entries 2 input)))
  (time (= 241861950 (find-entries 3 test-input)))
  (time (= 85491920 (find-entries 3 input)))
  )