(ns adventofcode.year2018.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn render [{:keys [units grid]}]
  (->> (reduce (fn [g [c v]] (assoc-in g c v)) grid units)
       (apply map vector)
       (map cs/join)
       (cs/join "\n")))

(defn parse-grid [input]
  (->> input
       io/resource
       slurp
       cs/split-lines
       (apply mapv vector)))

(defn find-elves-and-goblins [grid]
  (for [x (range (count grid)) y (range (count (grid x)))
        :let [v (get-in grid [x y])]
        :when (#{\E \G} v)]
    [[x y] v]))

(defn parse-input [input]
  (let [g (parse-grid input)
        units (find-elves-and-goblins g)
        grid (reduce (fn [g [c]] (assoc-in g c \.)) g units)]
    {:units units
     :grid  grid}))

(defn neighbors [grid [i j]]
  (let [is ((juxt identity dec inc identity) i)
        js ((juxt inc identity identity dec) j)]
    (filter (partial get-in grid) (map vector is js))))

(defn bf-step [{:keys [frontier visited grid] :as m}]
  (let [next-state (peek frontier)
        new-neighbors (remove #(contains? visited %) (neighbors grid next-state))]
    (-> m
        (update :frontier #(-> % pop (into new-neighbors)))
        (update :visited into (zipmap new-neighbors (repeat next-state))))))

(defn recover-path [{:keys [frontier visited]}]
  (vec (reverse (take-while some? (iterate visited (peek frontier))))))

(def input1 (parse-input "adventofcode/year2018/day15/input1.txt"))

(println (render input1))

(let [{:keys [units] :as ic} input1
      [[f] & r] (sort-by first units)
      goal (into {} r)]
  (->> (into ic {:frontier [f] :visited {f nil}})
       (iterate bf-step)
       (take-while (comp seq :frontier))
       (filter (fn [{:keys [frontier]}] (goal (peek frontier))))
       first
       (recover-path)
       second))