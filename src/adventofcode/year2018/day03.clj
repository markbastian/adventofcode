(ns adventofcode.year2018.day03
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as s]))

(def input "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2")
(def test-input (->> "adventofcode/year2018/day03/input.txt" io/resource slurp))

(defn parse-claims [input]
  (->> (re-seq #"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)" input)
       (map (fn [[_ c x y w h]]
              (zipmap [:claim :x :y :w :h]
                      (map edn/read-string [c x y w h]))))))

(defn overlapping-squares [input]
  (->>
    (for [{:keys [x y w h]} (parse-claims input)
          i (range x (+ x w))
          j (range y (+ y h))] [i j])
    frequencies
    (remove (comp zero? dec second))))

(defn overlapping-square-count [input]
  (count (overlapping-squares input)))

(comment
  (overlapping-square-count input)
  (time (= 105231 (overlapping-square-count test-input))))

(defn valid-claims [input]
  (let [f (->> (overlapping-squares input) (map first) set)]
    (first
      (for [{:keys [claim x y w h]} (->> input parse-claims)
            :let [coords (set (for [i (range x (+ x w)) j (range y (+ y h))] [i j]))]
            :when (empty? (s/intersection f coords))]
        {:claim claim}))))

(comment
  (time (= {:claim 164} (valid-claims test-input))))