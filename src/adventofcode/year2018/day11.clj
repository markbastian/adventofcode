(ns adventofcode.year2018.day11)

(def m
  [[31 2 4 33 5 36]
   [12 26 9 10 29 25]
   [13 17 21 22 20 18]
   [24 23 15 16 14 19]
   [30 8 28 27 11 7]
   [1 35 34 3 32 6]])

;https://en.wikipedia.org/wiki/Summed-area_table
(defn summed-area-table [m]
  (let [coords (for [i (range (count m)) j (range (count (m i)))] [i j])]
    (reduce (fn [I [i j :as coord]]
              (let [a (get-in m coord)
                    b (get-in I [(dec i) j] 0)
                    c (get-in I [i (dec j)] 0)
                    d (get-in I [(dec i) (dec j)] 0)]
                (assoc-in I coord (+ a b c (- d)))))
            m coords)))

(defn rect-sum [m [x0 y0 :as lo] [x1 y1 :as hi]]
  (let [A (get-in m lo)
        B (get-in m [x1 y0])
        C (get-in m [x0 y1])
        D (get-in m hi)]
    (- (+ A D) (+ B C))))

(defn area-sum [m [x y :as coord] dim]
  (rect-sum m coord [(+ x dim) (+ y dim)]))

(def sat (summed-area-table m))

(rect-sum sat [2 1] [4 4])

(rect-sum sat [0 0] [1 1])

(area-sum sat [0 0] 1)

(defn power [serno [x y]]
  (let [rack-id (+ x 10)
        power-level (* rack-id (+ serno (* rack-id y)))]
    (- (rem (quot power-level 100) 10) 5)))

(power 8 [3 5])
(power 57 [122 79])
(power 39 [217 196])
(power 71 [101 153])

(defn power-map [dim serno]
  (into {}
        (for [i (map inc (range dim)) j (map inc (range dim))
              :let [c [i j]]] [c (power serno c)])))

(defn power-grid [serno dim]
  (let [m (make-array Long/TYPE dim dim)]
    (doseq [i (range dim) j (range dim)]
      (aset m i j (power serno [i j])))
    (into [] (map vec m))))

(let [g (power-grid 42 300)
      t (summed-area-map g)]
  t)

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