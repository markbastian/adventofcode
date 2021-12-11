(ns adventofcode.year2021.day11
  (:require [adventofcode.input-util :as iu]))

(defn parse [s]
  (let [grid (->> (iu/read-as-strings s)
                  (mapv (fn [s] (mapv (fn [c] (- (int c) (int \0))) s))))]
    (into
      {}
      (for [i (range (count grid))
            j (range (count (grid i)))]
        [[i j] (get-in grid [i j])]))))

(def example-input (parse "adventofcode/year2021/day11/example-input.txt"))
(def sample-input (parse "adventofcode/year2021/day11/sample-input.txt"))
(def input (parse "adventofcode/year2021/day11/input.txt"))

(defn moore-neigbors [[x y]]
  (let [i ((juxt inc inc identity dec dec dec identity inc) x)
        j ((juxt identity inc inc inc identity dec dec dec) y)]
    (map vector i j)))

(defn flash-step [{:keys [grid flashed] :as state}]
  (let [new-flashes (->> (for [[coord energy] grid :when (> energy 9)] coord)
                         (remove flashed))
        spill (->> (mapcat moore-neigbors new-flashes)
                   (filter grid)
                   frequencies)]
    (-> state
        (update :grid #(merge-with + spill %))
        (update :flashed into new-flashes))))

(defn step [state]
  (let [{:keys [flashed] :as state} (->> (update state :grid update-vals inc)
                                         (iterate flash-step)
                                         (partition 2 1)
                                         (some (fn [[a b]] (when (= a b) a))))]
    (-> state
        (update :grid update-vals (fn [v] (if (> v 9) 0 v)))
        (update :flashed empty)
        (assoc :num-flashed (count flashed)))))

(defn part1 [input]
  (->> {:grid input :flashed #{}}
       (iterate step)
       rest
       (take 100)
       (map :num-flashed)
       (reduce +)))

(defn part2 [input]
  (->> {:grid input :flashed #{}}
       (iterate step)
       (map :num-flashed)
       (take-while (complement #{(count input)}))
       count))

(comment
  (= 1656 (part1 sample-input))
  (= 1723 (part1 input))
  (= 195 (part2 sample-input))
  (= 327 (part2 input))
  )