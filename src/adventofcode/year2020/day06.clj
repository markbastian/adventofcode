(ns adventofcode.year2020.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.set :refer [intersection]]))

(defn input->lines [resource]
  (->> resource io/resource slurp cs/split-lines))

(def test-input (input->lines "adventofcode/year2020/day06/test-input.txt"))

(def input (input->lines "adventofcode/year2020/day06/input.txt"))

(defn p [op input]
  (->> input (partition-by (comp some? seq)) (map op) (apply +)))

(def p1 (partial p (comp count distinct cs/join)))
(def p2 (partial p (fn [s] (->> s (map set) (apply intersection) count))))

(comment
  (time (= 11 (p1 test-input)))
  (time (= 6726 (p1 input))))

(comment
  (time (= 6 (p2 test-input)))
  (time (= 3316 (p2 input))))

