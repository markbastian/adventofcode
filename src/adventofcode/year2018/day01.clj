(ns adventofcode.year2018.day01
  (:require [clojure.java.io :as io]
            [clojure.core.reducers :as r]))

(defonce day-1-input (-> "adventofcode/year2018/day01/day0101.txt" io/resource slurp))
(defonce alternate-input (-> "adventofcode/year2018/day01/alternate.txt" io/resource slurp))

(defn op-seq [input]
  (->> input
       (re-seq #"[+-]\d+")
       (map #(Long/parseLong %))))

(defn freq-adjuster [s]
  (apply + (op-seq s)))

(comment
  (time (= 510 (freq-adjuster day-1-input)))
  (time (= 520 (freq-adjuster alternate-input))))

(defn freq-x2 [s]
  (->> s
       op-seq
       cycle
       (reductions +)
       (reduce (fn [v n] (if (v n) (reduced n) (conj v n))) #{})))

(comment
  (time (= 10 (freq-x2 "+3, +3, +4, -2, -4")))
  (time (= 69074 (freq-x2 day-1-input)))
  (time (= 394 (freq-x2 alternate-input))))

(defn freq-x2-faster [input]
  (let [v (vec (op-seq input))]
    (loop [visited (transient #{}) i 0 value (v i)]
      (if (visited value)
        value
        (let [i (mod (inc i) (count v))]
          (recur (conj! visited value) i (+ value (v i))))))))

(comment
  (time (= 69074 (freq-x2-faster day-1-input)))
  (time (= 394 (freq-x2-faster alternate-input))))