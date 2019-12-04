(ns adventofcode.year2019.day03
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

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

(defn intersections [path1 path2]
  (let [p1 (distinct path1)
        p2 (distinct path2)]
    (->> (into p1 p2)
         frequencies
         (remove (fn [[loc freq]] (or (= [0 0] loc) (= 1 freq))))
         (map first))))

(defn part1 [input1 input2]
  (let [path1 (wire-path input1)
        path2 (wire-path input2)]
    (->> (intersections path1 path2)
         (map (fn [[^int a ^int b]] (+ (Math/abs a) (Math/abs b))))
         (apply min))))

(defn steps [loc v]
  (first (filter #(= loc (v %)) (range (count v)))))

(defn part2 [input1 input2]
  (let [path1 (wire-path input1)
        path2 (wire-path input2)
        ix (intersections path1 path2)]
    (apply min
           (for [i ix :let [a (steps i path1) b (steps i path2)]]
             (+ a b)))))

(comment
  (intersections
    (wire-path "R75,D30,R83,U83,L12,D49,R71,U7,L72")
    (wire-path "U62,R66,U55,R34,D71,R55,D58,R83"))

  (part1 "R8,U5,L5,D3" "U7,R6,D4,L4")
  (part1 "R75,D30,R83,U83,L12,D49,R71,U7,L72"
         "U62,R66,U55,R34,D71,R55,D58,R83")
  (part1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
         "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

  ;Part 1: 207
  (= 207
     (->> (io/resource "adventofcode/year2019/day03/input.txt") slurp cs/split-lines (apply part1)))

  (= 30 (part2 "R8,U5,L5,D3" "U7,R6,D4,L4"))
  (= 610 (part2 "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                "U62,R66,U55,R34,D71,R55,D58,R83"))
  (= 410 (part2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
  ;Part 2: 21196
  (= 21196
     (->> (io/resource "adventofcode/year2019/day03/input.txt") slurp cs/split-lines (apply part2)))
  )