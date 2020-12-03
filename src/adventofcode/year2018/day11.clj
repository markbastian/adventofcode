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

(comment
  (= 4 (power 8 [3 5]))
  (= -5 (power 57 [122 79]))
  (= 0 (power 39 [217 196]))
  (= 4 (power 71 [101 153])))

(defn power-grid [serno dim]
  (let [m (make-array Long/TYPE dim dim)]
    (doseq [i (range dim) j (range dim)]
      (aset m i j (power serno [i j])))
    (into [] (map vec m))))

(defn largest-power-cell [sat dim]
  (let [grid-power (for [i (range (- 300 dim)) j (range (- 300 dim))]
                     [[(inc i) (inc j)] (area-sum sat [i j] dim)])]
    (apply max-key second grid-power)))

(defn part1 [serno sz]
  (let [sat (summed-area-table (power-grid serno 300))]
    (largest-power-cell sat sz)))

(comment
  (= [[33 45] 29] (part1 18 3))
  (= [[21 61] 30] (part1 42 3))
  (= [[243 27] 30] (part1 6303 3)))

(defn part2 [serno]
  (let [sat (summed-area-table (power-grid serno 300))
        powers (for [sz (range 300)]
                 (conj (largest-power-cell sat sz) sz))]
    (apply max-key second powers)))

(comment
  (= [[90 269] 113 16] (part2 18))
  (= [[232 251] 119 12] (part2 42))
  (= [[284 172] 88 12] (part2 6303)))