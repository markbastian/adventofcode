(ns adventofcode.year2017.day06)

(def input [0 2 7 0])

(defn step [input]
  (let [[f & r] (map vector (range) input)
        [s n] (reduce (fn [[_ v0 :as a] [_ v1 :as b]] (if (> v1 v0) b a)) f r)]
    (reduce #(update %1 %2 inc) (assoc input s 0) (map #(mod (inc (+ s %)) (count input)) (range 0 n)))))

(defn cycle-map [input]
  (->> (iterate step input)
       (partition 2 1)
       (reductions
         (fn [m [k v]]
           (if (m k) (reduced m) (assoc m k v)))
         {})
       last))

(defn cycle-count [input]
  (->> input cycle-map count))

(comment
  ;5
  (cycle-count input))

(comment
  ;4074
  (cycle-count
    [11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11]))

(defn cycle-count-2 [input]
  (let [m (cycle-map input)
        c (some (fn [[k v]] (when (= 2 v) k)) (frequencies (vals m)))
        [f & r] (iterate m c)]
    (count (cons f (take-while (complement #{f}) r)))))

(comment
  ;2793
  (cycle-count-2 [11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11]))