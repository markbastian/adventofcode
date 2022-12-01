(ns adventofcode.year2022.day01
  (:require [adventofcode.input-util :as iu]))

(def sample-input
  (iu/read-as-strings "adventofcode/year2022/day01/sample-input.txt"))

(def input
  (iu/read-as-strings "adventofcode/year2022/day01/input.txt"))

(defn part1 [input]
  (->> input
       (map parse-long)
       (partition-by nil?)
       (remove #(every? nil? %))
       (map #(reduce + %))
       (apply max)))

(defn part2 [input]
  (->> input
       (map parse-long)
       (partition-by nil?)
       (remove #(every? nil? %))
       (map #(reduce + %))
       (sort #(- %2 %1))
       (take 3)
       (reduce +)))

(comment
  (= 24000 (part1 sample-input))
  (= 70116 (part1 input))

  (= 45000 (part2 sample-input))
  (= 206582 (part2 input)))

