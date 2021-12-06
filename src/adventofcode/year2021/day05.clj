(ns adventofcode.year2021.day05
  (:require [adventofcode.input-util :as iu]
            [clojure.string :as str]))

(defn parse [input]
  (->> input
       iu/read-as-strings
       (mapv (fn [line] (->> (str/split (str/replace line #"\s*->\s*" ",") #",")
                             (map parse-long)
                             (partition 2)
                             (mapv vec))))))

(def sample-input (parse "adventofcode/year2021/day05/sample-input.txt"))
(def input (parse "adventofcode/year2021/day05/input.txt"))

(defn dir [[u v]]
  (let [[dx dy] (mapv - v u)]
    [(Long/signum dx) (Long/signum dy)]))

(defn make-line [[i f dir]]
  (let [line (->> (iterate #(mapv + % dir) i)
                  (take-while (complement #{f}))
                  vec)]
    (conj line f)))

(defn overlaps [input remove-diagonals?]
  (let [input (cond->> (map (fn [pt] (conj pt (dir pt))) input)
                       remove-diagonals?
                       (filter (fn [[_ _ [a b]]] (zero? (* a b)))))]
    (->> input
         (mapcat make-line)
         frequencies
         vals
         (filter (comp pos? dec))
         count)))

(comment
  (= 5 (overlaps sample-input true))
  (= 7644 (overlaps input true))
  (= 12 (overlaps sample-input false))
  (= 18627 (overlaps input false))
  )
