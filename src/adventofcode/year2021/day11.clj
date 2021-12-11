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
  (let [new-flashes (for [[coord energy] grid
                          :when (> energy 9)
                          :when (not (flashed coord))]
                      coord)
        spillover (->> (mapcat moore-neigbors new-flashes)
                       (filter grid)
                       frequencies)]
    (-> state
        (update :grid #(merge-with + spillover %))
        (update :flashed into new-flashes))))

(defn propagate-flashes [state]
  (letfn [(solution [[last-step next-step]]
            (when (= last-step next-step)
              last-step))]
    (->> state
         (iterate flash-step)
         (partition 2 1)
         (some solution))))

(defn step [state]
  (-> state
      (update :grid update-vals inc)
      (update :flashed empty)
      propagate-flashes
      (update :grid update-vals (fn [v] (if (> v 9) 0 v)))))

(defn flash-count-seq [input]
  (->> {:grid input :flashed #{}}
       (iterate step)
       (map (comp count :flashed))))

(defn part1 [input]
  (->> input
       flash-count-seq
       rest
       (take 100)
       (reduce +)))

(defn part2 [input]
  (->> input
       flash-count-seq
       (take-while (complement #{(count input)}))
       count))

(comment
  (= 1656 (part1 sample-input))
  (= 1723 (part1 input))
  (= 195 (part2 sample-input))
  (= 327 (part2 input))
  )