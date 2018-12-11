(ns adventofcode.year2018.day11)

(defn power [serno [x y]]
  (let [rack-id (+ x 10)
        power-level (* rack-id (+ serno (* rack-id y)))]
    (- (rem (quot power-level 100) 10) 5)))

(power 8 [3 5])
(power 57 [122 79])
(power 39 [217 196])
(power 71 [101 153])

(defn largest-power-grid [serno sz]
  (let [dim 300
        dim2 (- dim sz)
        m (into {} (for [i (map inc (range dim)) j (map inc (range dim))
                         :let [c [i j]]] [c (power serno c)]))
        powers (for [x0 (map inc (range dim2)) y0 (map inc (range dim2))]
                 [[x0 y0] (reduce
                            +
                            (for [i (range sz) j (range sz)]
                              (m [(+ x0 i) (+ y0 j)])))])]
    (apply max-key second powers)))

(comment
  (largest-power-grid 18 3)
  (largest-power-grid 42 3)
  (largest-power-grid 6303 3))

(comment
  ;This will take forever. Instead consider an "advancing front" from each cell.
  ;Would another memoizing strategy work?
  (for [i (map inc (range 300))]
    [i (largest-power-grid 6303 i)]))