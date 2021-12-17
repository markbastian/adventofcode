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

(defn moore-neigbors [[x y]]
  (let [i ((juxt inc inc identity dec dec dec identity inc) x)
        j ((juxt identity inc inc inc identity dec dec dec) y)]
    (map vector i j)))

(defn center [[[x-min x-max] [y-min y-max]]]
  [(quot (+ x-min x-max) 2)
   (quot (+ y-min y-max) 2)])

(defn solve-step [{:keys [best-ic visited best-height] :as state}]
  (let [neighbors (->> best-ic
                       :velocity
                       moore-neigbors
                       (remove visited))
        trajectories (for [v neighbors
                           :let [ic (assoc best-ic :velocity v)
                                 trajectory (compute-trajectory ic)
                                 solution-type (categorize-solution (last trajectory))]
                           :when (= :hit solution-type)
                           :let [[min-y max-y] (y-bounds trajectory)]]
                       {:ic ic :max-y max-y})]
    (reduce
      (fn [{:keys [best-height] :as state} {:keys [ic max-y]}]
        (if (> max-y best-height)
          (assoc state :best-ic ic :best-height max-y)
          state))
      (update state :visited into neighbors)
      trajectories)))

(comment
  (let [bounds input
        [dx dy] (center bounds)
        c [(quot dx 10) (- dy)]]
    (->> {:best-ic     {:position [0 0]
                        :velocity c
                        :bounds   bounds}
          :visited     #{c}
          :best-height ##-Inf}
         (iterate solve-step)
         (partition 2 1)
         (drop-while (fn [[{a :best-height} {b :best-height}]] (not= a b)))
         ffirst
         ))

  (count
    (let [[[min-x max-x] [min-y max-y] :as bounds] input
          max-y-vel max-x]
      (for [vx (map inc (range max-x))
            vy (range min-y (inc 135))
            :let [trajectory (compute-trajectory {:position [0 0]
                                                  :velocity [vx vy]
                                                  :bounds   bounds})]
            :when (= :hit (categorize-solution (last trajectory)))]
        [vx vy])))
  )


