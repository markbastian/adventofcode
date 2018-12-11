(ns adventofcode.year2018.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.edn :as edn]))

(defn parse [s]
  (->> (re-seq #"<[^>]+>" s)
       (mapv #(edn/read-string (str "[" (cs/replace % #"[<>\s]+" "") "]")))
       (partition 2)
       (mapv vec)))

(def test-input
  (-> "adventofcode/year2018/day10/test_input.txt" io/resource slurp parse))

(def input
  (-> "adventofcode/year2018/day10/input.txt" io/resource slurp parse))

(defn step [[p v]] [(mapv + p v) v])

(defn all-step [v] (mapv step v))

(defn bounds [pts]
  (let [x ((juxt (fn [v] (apply min v)) (fn [v] (apply max v))) (map first pts))
        y ((juxt (fn [v] (apply min v)) (fn [v] (apply max v))) (map second pts))]
    [x y]))

(defn render [pts]
  (let [[minx maxx] ((juxt (fn [v] (apply min v)) (fn [v] (apply max v))) (map first pts))
        [miny maxy] ((juxt (fn [v] (apply min v)) (fn [v] (apply max v))) (map second pts))
        grid (vec (repeat (inc (- maxx minx)) (vec (repeat (inc (- maxy miny)) \space))))]
    ;;9 determined from experimentation
    (when (= 9 (- maxy miny))
      (->> (reduce (fn [g p] (assoc-in g (mapv - p [minx miny]) \X)) grid pts)
           (apply mapv vector)
           (map cs/join)
           (cs/join "\n")))))

(comment
  (->>
    test-input
    (iterate all-step)
    (map (partial mapv first))
    (map render)
    (take 4))

  10519
  (->>
    input
    (iterate all-step)
    (map (partial mapv first))
    ;(filter (fn [m] (let [[[minx maxx] [miny maxy]] (bounds m)]
    ;                  (< (+ (- maxx minx) (- maxy miny)) 500))))
    (map render)
    (map vector (range))
    (filter second)
    first))