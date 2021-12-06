(ns adventofcode.year2021.day06
  (:require [adventofcode.input-util :as iu]))

(def sample-input (iu/read-as-array "adventofcode/year2021/day06/sample-input.txt"))
(def input (iu/read-as-array "adventofcode/year2021/day06/input.txt"))

;; Naive solution
(defn step [input]
  (let [s (mapv dec input)
        spawns (repeat (count (filter neg? s)) 8)]
    (into (mapv (fn [v] (if (neg? v) 6 v)) s) spawns)))

(defn part1 [input days]
  (let [f (frequencies input)
        x (->> (iterate step [5])
               (map count)
               (drop days)
               (take 5)
               (zipmap [5 4 3 2 1]))]
    (->> (map (fn [[k v]] (* v (f k 0))) x)
         (reduce +))))

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

(comment
  (= 5934 (part1 sample-input 80))
  (= 374927 (part1 input 80))

  (= 5934 (fast-solution sample-input 80))
  (= 374927 (fast-solution input 80))
  (= 26984457539 (fast-solution sample-input 256))
  (= 1687617803407 (fast-solution input 256))
  )
