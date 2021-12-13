(ns adventofcode.year2021.day13
  (:require [adventofcode.input-util :as iu]
            [clojure.string :as str]))

(defn parse-input [input]
  (let [[coords [_ & folds]] (->> (iu/read-as-strings input)
                                  (split-with (complement empty?)))
        coords (set (map (fn [line] (mapv parse-long (str/split line #","))) coords))]
    {:coords coords
     :width  (apply max (map first coords))
     :height (apply max (map second coords))
     :folds  (mapv
               (fn [line]
                 (let [[_ axis value] (re-matches #"fold along (.)=(\d+)" line)]
                   [(keyword axis) (parse-long value)]))
               folds)}))

(def sample-input (parse-input "adventofcode/year2021/day13/sample-input.txt"))
(def input (parse-input "adventofcode/year2021/day13/input.txt"))

(defn to-grid [{:keys [coords width height]}]
  (mapv
    (fn [y]
      (mapv
        (fn [x]
          (if (get coords [x y]) \# \space))
        (range (inc width))))
    (range (inc height))))

(defn draw-grid [input]
  (->> input to-grid (map str/join) (str/join "\n")))

(def print-grid (comp println draw-grid))

(defn split-coords [{[[axis value]] :folds :keys [coords]}]
  (let [split-fn ({:x first :y second} axis)]
    (->> coords
         (remove (comp #{value} split-fn))
         (group-by (fn [coord]
                     (if (> (split-fn coord) value)
                       :below
                       :above))))))

(defn fold-once [{[[axis value]] :folds :as state}]
  (letfn [(mirror-value [v] (- value (- v value)))
          (mirror-coord [coord] (update coord ({:x 0 :y 1} axis) mirror-value))]
    (let [{:keys [below above]} (split-coords state)
          folded (mapv mirror-coord below)]
      (-> state
          (assoc :coords (set (into above folded)))
          (assoc ({:x :width :y :height} axis) (dec value))
          (update :folds rest)))))

(defn fold-all [input]
  (->> input
       (iterate fold-once)
       (drop-while (comp seq :folds))
       first))

(comment
  (= 17 (-> sample-input fold-once :coords count))
  (= 785 (-> input fold-once :coords count))

  ;; 0
  (->> sample-input fold-all print-grid)

  ;; FJAHJGAH
  (->> input fold-all print-grid)
  )