(ns adventofcode.year2019.day03
  (:require [clojure.string :as cs]))

(def dir-map {\U [0 1] \D [0 -1] \L [-1 0] \R [1 0]})

(defn dir-step [cmd path]
  (let [direction (dir-map (first cmd))
        delta (Long/parseLong (subs cmd 1))]
    (reduce (fn [path dir] (conj path (mapv + (peek path) dir))) path (repeat delta direction))))

(defn step [[[f & r] path]]
  (if f [r (dir-step f path)] [nil path]))

(defn wire-path [input]
  (->> [(->> (cs/split input #",") (map cs/trim)) [[0 0]]]
       (iterate step)
       (remove first)
       first
       second))

(defn part1 [input1 input2]
  (let [path1 (distinct (wire-path input1))
        path2 (distinct (wire-path input2))]
    (->> (into path1 path2)
         frequencies
         (remove (fn [[loc freq]] (or (= [0 0] loc) (= 1 freq))))
         (map first)
         (map (fn [[a b]] (+ a b)))
         (apply min))))

(part1 "R8,U5,L5,D3" "U7,R6,D4,L4")
(part1 "R75,D30,R83,U83,L12,D49,R71,U7,L72"
       "U62,R66,U55,R34,D71,R55,D58,R83")