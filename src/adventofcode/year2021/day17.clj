(ns adventofcode.year2021.day17)

(defn parse-input [s]
  (let [[x-min x-max y-min y-max] (->> (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" s)
                                       rest
                                       (map parse-long))]
    [[x-min x-max] [y-min y-max]]))

(def sample-input (parse-input "target area: x=20..30, y=-10..-5"))
(def input (parse-input "target area: x=150..193, y=-136..-86"))

(defn categorize-solution [{[px py]                       :position
                            [[x-min x-max] [y-min y-max]] :bounds}]
  (cond
    (and (<= x-min px x-max) (<= y-min py y-max)) :hit
    (or (> px x-max) (< py y-min)) :miss
    :else :in-flight))

(defn step [{p :position [vx vy :as v] :velocity :as state}]
  (assoc state
    :position (mapv + p v)
    :velocity [(- vx (Long/signum vx)) (dec vy)]))

(defn compute-trajectory [ic]
  (->> ic
       (iterate step)
       (take-while (comp (complement #{:miss}) categorize-solution))))

(defn y-bounds [trajectory]
  (reduce
    (fn [[min-y max-y] {[_ y] :position}]
      [(min y min-y) (max y max-y)])
    [Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY]
    trajectory))

(defn all-solutions [input]
  (let [[[_min-x max-x] [min-y _max-y] :as bounds] input]
    (for [vx (map inc (range max-x))
          vy (range min-y (- min-y))
          :let [trajectory (compute-trajectory {:position [0 0]
                                                :velocity [vx vy]
                                                :bounds   bounds})]
          :when (= :hit (categorize-solution (last trajectory)))
          :let [[min-y max-y] (y-bounds trajectory)]]
      [vx vy max-y])))
(comment
  (= 45 (->> (all-solutions sample-input) (sort-by (comp - last)) first last))
  (= 9180 (->> (all-solutions input) (sort-by (comp - last)) first last))

  (= 112 (count (all-solutions sample-input)))
  (time (= 3767 (count (all-solutions input))))
  )


