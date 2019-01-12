(ns adventofcode.year2018.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.edn :as edn]))

(defn parse-line [s]
  (->> (cs/split s #",")
       (map cs/trim)
       (map #(cs/split % #"="))
       (sort-by first)
       (map second)
       (map #(map edn/read-string (cs/split % #"\.+")))
       (map (fn [[a b]] (if b (range a (inc b)) (repeat a))))
       (apply map vector)))

(defn clay-cells [input]
  (->> input
       io/resource
       slurp
       cs/split-lines
       (mapcat parse-line)))

(defn bounds [cells]
  (let [xs (map first cells)
        ys (map second cells)]
    ;Bump out x to allow for water overflow
    [[(dec (apply min xs)) (inc (apply max xs))]
     [(apply min ys) (apply max ys)]]))

(defn build-grid [[[minx maxx] [miny maxy]]]
  (let [w (inc (- maxx minx))
        h (inc (- maxy miny))]
    (vec (repeat h (vec (repeat w \.))))))

(defn set-cell [[[minx] [miny]] grid [x y]]
  (assoc-in grid [(- y miny) (- x minx)] \#))

(defn render [g]
  (cs/join "\n" (map cs/join g)))

(defn flood [g [y x :as c]]
  (let [d [(inc y) x]
        l [y (dec x)]
        r [y (inc x)]]
    (cond
      (= \. (get-in g d)) (flood g d)
      (= \. (get-in g l)) (flood (assoc-in g c \~) l)
      (= \. (get-in g r)) (flood (assoc-in g c \~) r)
      (nil? (get-in g r)) g
      :else (assoc-in g c \~))))

(let [cells (clay-cells "adventofcode/year2018/day17/input1.txt")
      [[minx] [miny] :as bounds] (bounds cells)
      grid (reduce (partial set-cell bounds) (build-grid bounds) cells)
      spigot (reverse (mapv - [500 1] [minx miny]))]
  (->> (reductions flood grid (repeat spigot))
       (take 10)
       (map render))
  #_(->> (flood grid spigot)
       render
       println))