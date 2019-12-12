(ns adventofcode.year2019.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn parse-input [s]
  (->> s
       cs/split-lines
       (map (partial re-matches #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>"))
       (map rest)
       (mapv (fn [v] [(mapv #(Long/parseLong %) v) [0 0 0]]))))

(def example (-> "adventofcode/year2019/day12/example.txt" io/resource slurp parse-input))
(def input (-> "adventofcode/year2019/day12/input.txt" io/resource slurp parse-input))

(defn sign [a b]
  (cond
    (> a b) 1
    (< a b) -1
    :else 0))

(defn gravities [v]
  (for [i (range (count v))
        :let [[pi] (v i)]
        j (range (count v))
        :let [[pj] (v j)]
        :when (not= i j)
        :let [g (mapv sign pj pi)]]
    [i g]))

(defn step [v]
  (letfn [(apply-grav [v [i gi]] (update-in v [i 1] (fn [vel] (mapv + vel gi))))
          (update-pos [[pos vel]] [(mapv + pos vel) vel])]
    (->> (gravities v)
         (reduce apply-grav v)
         (mapv update-pos))))

(defn total-energy [v]
  (reduce
    (fn [energy [pos vel]]
      (let [potential (reduce + (map #(Math/abs ^long %) pos))
            kinetic (reduce + (map #(Math/abs ^long %) vel))]
        (+ energy (* potential kinetic))))
    0
    v))

(comment
  (->> example
       (iterate step)
       (take 11)
       (map total-energy))

  (->> input
       (iterate step)
       (drop 1000)
       first
       total-energy)

  (->> example
       (iterate step)
       rest
       (take-while (complement #{example}))
       count
       inc)

  (->> input
       (iterate step)
       (map total-energy)
       (take 2772)
       frequencies
       (sort-by second))
  )

