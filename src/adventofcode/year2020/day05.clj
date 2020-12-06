(ns adventofcode.year2020.day05
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def input (->> "adventofcode/year2020/day05/input.txt" io/resource slurp cs/split-lines))
(def input-1 "FBFBBFFRLR")

(defn bisect [[lo hi] l]
  (case l
    (\B \R) [(inc (quot (+ lo hi) 2)) hi]
    (\L \F) [lo (quot (+ lo hi) 2)]))

(defn seat [input]
  (let [[row] (reduce bisect [0 127] (take 7 input))
        [seat] (reduce bisect [0 7] (drop 7 input))]
    {:row row :seat seat}))

(defn seat-hash [input]
  (let [{:keys [row seat]} (seat input)]
    (+ (* row 8) seat)))

(comment
  (time (= 357 (seat-hash input-1)))
  (time (= 567 (seat-hash "BFFFBBFRRR")))
  (time (= 119 (seat-hash "FFFBBBFRRR")))
  (time (= 820 (seat-hash "BBFFBBFRLL")))

  (time (= 922 (apply max (map seat-hash input))))

  (time
    (->> (sort (map seat-hash input))
         (partition 2 1)
         (some (fn [[lo hi]] (when (= 2 (- hi lo)) (inc lo))))))
  )