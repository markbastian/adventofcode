(ns adventofcode.year2019.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn decode [s w h]
  (->> s
       (map #(Long/parseLong (str %)))
       (partition-all w)
       (partition-all h)))

(comment
  ;Part 1: 2250
  (let [input (slurp (io/resource "adventofcode/year2019/day08/input.txt"))]
    (->> (decode input 25 6)
         (map (fn [v] (frequencies (flatten v))))
         (sort-by (fn [m] (m 0)))
         (map (fn [m] (assoc m :1x2 (* (m 1) (m 2)))))
         first))

  ;FHJUL
  (let [input (slurp (io/resource "adventofcode/year2019/day08/input.txt"))
        images (mapv (fn [v] (mapv vec v)) (decode input 25 6))]
    (cs/replace
      (->> (for [c (range 6)]
             (for [r (range 25)]
               (some (comp #{0 1} (fn [layer] (get-in images [layer c r]))) (range (count images)))))
           (map (partial apply str))
           (cs/join "\n"))
      #"0" " "))
  )
