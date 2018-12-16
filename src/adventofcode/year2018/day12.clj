(ns adventofcode.year2018.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn read-input [txt]
  (let [[f & r] (cs/split-lines txt)
        [_ s] (cs/split f #":")]
    [0
     (cs/trim s)
     (->> r
          (map #(re-matches #"([#\.]{5})\s*=>\s*([#\.])" %))
          (filter identity)
          (map (fn [[_ s r]] [(seq s) r]))
          (filter (fn [[_ r]] (= r "#")))
          (map first)
          set)]))

(def input1 (-> "adventofcode/year2018/day12/test_input.txt" io/resource slurp read-input))
(def input (-> "adventofcode/year2018/day12/input.txt" io/resource slurp read-input))

(defn step [[n s f]]
  (let [s (partition 5 1 (seq (format "...%s..." s)))]
    [(dec n) (cs/join (map #(if (f %) \# \.) s)) f]))

;(count (step input1))

(defn compute [ngens input]
  (->> (iterate step input)
       (take (inc ngens))
       (map (fn [[n s]] (map (fn [i s] (if (= \# s) i 0)) (iterate inc n) s)))
       (map #(reduce + %))
       last))

(compute 20 input1)
(compute 20 input)
;(compute 50000000000 input)

#_
(->> (iterate step input)
     (take (inc 50))
     (map (fn [[n s]] (map (fn [i s] (if (= \# s) i 0)) (iterate inc n) s)))
     (map #(reduce + %))
     (partition 2 1)
     (map (fn [[a b]] (- a b)))
     frequencies)