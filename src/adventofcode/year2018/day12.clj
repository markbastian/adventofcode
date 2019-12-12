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
  (let [s (partition 5 1 (seq (format "...%s..." s)))
        r (cs/join (map #(if (f %) \# \.) s))
        [_ pre pots] (re-matches #"(\.*)(.+?)\.*" r)]
    [(+ (dec n) (count pre)) pots f]))

(defn compute [ngens input]
  (->> (iterate step input)
       (take (inc ngens))
       (map (fn [[n s]] (map (fn [i s] (if (= \# s) i 0)) (iterate inc n) s)))
       (map #(reduce + %))
       last))

(comment
  (compute 20 input1)
  (compute 20 input)

  (->> (iterate step input)
       (map-indexed (fn [i [n s]] [i (reduce + (map (fn [i s] (if (= \# s) i 0)) (iterate inc n) s))]))
       (partition 2 1)
       (map (fn [[[x0 y0] [x1 y1]]]
              (let [m (/ (- y1 y0) (- x1 x0))
                    b (- y0 ( * m x0))]
                {:x0 x0 :y0 y0 :x1 x1 :y1 y1 :m m :b b})))
       (partition 2 1)
       (remove (fn [[a b]] (not= (select-keys a [:m :b]) (select-keys b [:m :b]))))
       ffirst)

  ;Part 2 solution
  (= 4350000000957 (+ (* 87 50000000000) 957))
  )