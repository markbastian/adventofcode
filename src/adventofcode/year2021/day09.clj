(ns adventofcode.year2021.day09
  (:require [adventofcode.input-util :as iu]))

(defn parse [s]
  (->> (iu/read-as-strings s)
       (mapv (fn [s] (mapv (fn [c] (- (int c) (int \0))) s)))))

(def sample-input (parse "adventofcode/year2021/day09/sample-input.txt"))
(def input (parse "adventofcode/year2021/day09/input.txt"))

(defn grid-neigbors [neighbors grid c]
  (filter (partial get-in grid) (neighbors c)))

(defn von-neumann-neighbors
  ([[x y]]
   (let [i ((juxt inc identity dec identity) x)
         j ((juxt identity inc identity dec) y)]
     (map vector i j)))
  ([grid c] (grid-neigbors von-neumann-neighbors grid c)))

(defn low-points [input]
  (->> (for [i (range (count input))
             j (range (count (input i)))
             :let [v (get-in input [i j])
                   n (von-neumann-neighbors input [i j])
                   vs (map (partial get-in input) n)]
             :when (every? #(< v %) vs)]
         [i j])))

(defn part1 [input]
  (->> input
       low-points
       (map (comp inc (partial get-in input)))
       (reduce +)))

(defn basin-step [{:keys [input pts visited] :as state}]
  (let [pt (peek pts)
        v (get-in input pt)
        new-points (->> (von-neumann-neighbors input pt)
                        (remove visited)
                        (filter (fn [pt]
                                  (let [nv (get-in input pt)]
                                    (< v nv 9)))))]
    (-> state
        (update :pts (fn [pts] (into (pop pts) new-points)))
        (update :visited conj pt))))

(defn basin-size [input start]
  (->> {:input input :pts [start] :visited #{}}
       (iterate basin-step)
       (drop-while (comp seq :pts))
       first
       :visited
       count))

(defn part2 [input]
  (->> input
       low-points
       (map (partial basin-size input))
       (sort >)
       (take 3)
       (apply *)))

(comment
  (= 15 (part1 sample-input))
  (= 528 (part1 input))
  (= 1134 (part2 sample-input))
  (= 920448 (part2 input))
  )