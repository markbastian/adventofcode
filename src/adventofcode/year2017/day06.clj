(ns adventofcode.year2017.day06)

(def input [0 2 7 0])

(defn f [input]
  (let [[s n] (apply max-key second (map vector (range) input))]
  (reduce #(update %1 %2 inc) input (map #(mod (inc (+ s %)) (count input)) (range 0 n)))))

(f input)