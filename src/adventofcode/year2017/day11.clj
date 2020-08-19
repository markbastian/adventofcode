(ns adventofcode.year2017.day11
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

;See https://github.com/markbastian/hex/blob/master/src/cljc/hex/core.cljc
; https://www.redblobgames.com/grids/hexagons/

(defn cube "Conform input to cube (3 element) representation of hex coords."
  [[i j k :as c]]
  (if k c [i j (- (+ i j))]))

(defn cube-dist [hex-a hex-b]
  (apply max (map #(Math/abs ^long (- %1 %2)) (cube hex-a) (cube hex-b))))

(defn step+x [[i j]] [(inc i) (dec j)])
(defn step-x [[i j]] [(dec i) (inc j)])
(defn step+y [[i j]] [i (inc j)])
(defn step-y [[i j]] [i (dec j)])
(defn step+z [[i j]] [(dec i) j])
(defn step-z [[i j]] [(inc i) j])

(def coords
  {"ne" step-z "sw" step+z "n" step+y "s" step-y "se" step+x "nw" step-x})

(defn str->ops [s]
  (map coords (cs/split s #",")))

(defn final-distance [s]
  (cube-dist [0 0] (reduce (fn [c op] (op c)) [0 0] (str->ops s))))

(defn path [s]
  (reductions (fn [c op] (op c)) [0 0] (str->ops s)))

(defn furthest-distance [[f & r :as _path]]
  (apply max (map (partial cube-dist f) r)))

(def input (slurp (io/resource "adventofcode/year2017/day11/input.txt")))

(comment
  ;Part 1
  (= 3 (final-distance "ne,ne,ne"))
  (= 0 (final-distance "ne,ne,sw,sw"))
  (= 2 (final-distance "ne,ne,s,s"))
  (= 3 (final-distance "se,sw,se,sw,sw"))
  (time (= 810 (final-distance input)))

  ;Part 2
  (time (= 1567 (furthest-distance (path input)))))

