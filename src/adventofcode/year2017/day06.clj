(ns adventofcode.year2017.day06)

(defn pair-to-spread [input]
  (let [[f & r] (map vector (range) input)]
    (reduce (fn [[_ v0 :as a] [_ v1 :as b]] (if (> v1 v0) b a)) f r)))

(defn step [input]
  (let [[spread-index blocks] (pair-to-spread input)]
    (letfn [(wrap-index [i] (mod (inc (+ spread-index i)) (count input)))]
      (reduce
        (fn [banks i] (update banks i inc))
        (assoc input spread-index 0)
        (map wrap-index (range 0 blocks))))))

(defn cycle-map [input]
  (letfn [(reducer [m [k v]] (if (m k) (reduced m) (assoc m k v)))]
    (->> (iterate step input)
         (partition 2 1)
         (reductions reducer {})
         last)))

(def cycle-count (comp count cycle-map))

(def test-input [0 2 7 0])
(def final-input [11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11])

(comment
  ;5
  (cycle-count test-input)
  ;4074
  (time (cycle-count final-input)))

(defn cycle-count-2 [input]
  (let [m (cycle-map input)
        cycle-start (some (fn [[k v]] (when (= 2 v) k)) (frequencies (vals m)))
        [f & r] (iterate m cycle-start)]
    (count (cons f (take-while (complement #{f}) r)))))

(comment
  (time (cycle-count-2 test-input))
  ;2793
  (time (cycle-count-2 final-input)))