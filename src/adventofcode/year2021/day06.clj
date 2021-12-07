(ns adventofcode.year2021.day06
  (:require [adventofcode.input-util :as iu]))

(def sample-input (iu/read-as-array "adventofcode/year2021/day06/sample-input.txt"))
(def input (iu/read-as-array "adventofcode/year2021/day06/input.txt"))

;; Efficient solution
(defn fast-step [input]
  (let [{new-gen -1 :as m} (update-keys input dec)]
    (cond-> m
            new-gen
            (->
              (update 6 (fnil + 0) new-gen)
              (update 8 (fnil + 0) new-gen)
              (dissoc -1)))))

(defn fast-solution [input days]
  (->> (frequencies input)
       (iterate fast-step)
       (drop days)
       first
       vals
       (reduce +)))

;;;Another version that is even faster and more concise
(defn fast-solution1 [input days]
  (->> (frequencies input)
       (reduce (fn [v [n f]] (assoc v n f)) (vec (repeat 9 0)))
       (iterate (fn [[f :as v]] (update (conj (subvec v 1) f) 6 + f)))
       (drop days)
       first
       (reduce +)))

(comment
  (= 5934 (fast-solution sample-input 80))
  (= 374927 (fast-solution input 80))
  (= 26984457539 (fast-solution sample-input 256))
  (= 1687617803407 (fast-solution input 256))

  (= 5934 (fast-solution1 sample-input 80))
  (= 374927 (fast-solution1 input 80))
  (= 26984457539 (fast-solution1 sample-input 256))
  (= 1687617803407 (fast-solution1 input 256))
  )
