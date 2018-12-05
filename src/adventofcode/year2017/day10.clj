(ns adventofcode.year2017.day10)

(def l (range 5))

(def input [3 4 1 5])

(defn step [[skip cp s] len]
  (let [n (count s)
        d (+ skip len)
        [f r] (split-at len (cycle s))]
    [(inc skip) (+ cp d) (take n (drop d (cycle (take n (into r f)))))]))

(defn checksum [l input]
  (let [[p cp s] (reduce step [0 0 l] input)]
    (prn [p cp (mod cp (count s))])
    (apply * (take 2 (drop (rem (count s) p) (cycle s))))))

(checksum l input)
;Not 402
(checksum
  [227 169 3 166 246 201 0 47 1 255 2 254 96 3 97 144]
  (range 256))

(reductions
  step
  [0 0 (range 5)]
  input)

(reductions
  step
  [0 0 (range 256)]
  [227 169 3 166 246 201 0 47 1 255 2 254 96 3 97 144])