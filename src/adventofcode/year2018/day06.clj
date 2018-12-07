(ns adventofcode.year2018.day06
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.data :refer [diff]]))

(def input
  "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")

(defn neighbors [[i j]]
  (let [xs ((juxt inc identity dec identity) i)
        ys ((juxt identity inc identity dec) j)]
    (mapv vector xs ys)))

(defn overlaps [n]
  (->> (map first n)
       frequencies
       (remove (fn [[_ freq]] (= 1 freq)))
       (map first)))

(defn step [m]
  (let [n (distinct (for [[k v] m n (neighbors k) :when (not (m n))] [n v]))]
    (conj
      (into {} n)
      (zipmap (overlaps n) (repeat nil))
      m)))

(defn simpify [m]
  (let [m (dissoc (group-by second m) nil)]
    (zipmap (keys m)
            (map count (vals m)))))

(defn input->pts [input]
  (->> input (format "[%s]") edn/read-string (partition 2) (mapv vec)))

(defn solution [input]
  (let [p (input->pts input)]
    (->> (zipmap p (range))
         (iterate step)
         (map simpify)
         (partition 2 1)
         (map (fn [[a b]] (last (diff a b))))
         (filter identity)
         (partition 20 1)
         (some (fn [v] (when (apply = v) (first v)))))))

(defn dist [[a b] [c d]]
  (+ (Math/abs ^int (- a c))
     (Math/abs ^int (- b d))))

(comment
  (solution input)
  ;2906
  (->> "adventofcode/year2018/day06/input.txt" io/resource slurp solution))

(defn roi-step [{:keys [frontier visited]}]
  (let [new-frontier (distinct (remove visited (mapcat neighbors frontier)))]
    {:frontier new-frontier
     :visited  (into visited frontier)}))

(defn total-dist [c pts]
  (reduce + (for [p pts] (dist p c))))

(defn filter-frontier [pts tol frontier]
  (->> frontier
       (map (fn [p] (total-dist p pts)))
       (filter (fn [d] (< d tol)))))

(defn region-size [tol input]
  (let [pts (input->pts input)
        s (reduce (fn [a b] (map + a b)) pts)
        c (mapv #(quot % (count pts)) s)]
    (->> {:frontier [c] :visited #{}}
         (iterate roi-step)
         (map :frontier)
         (map (partial filter-frontier pts tol))
         (take-while seq)
         flatten
         count)))

(comment
  ;16
  (region-size 32 input)
  ;50530
  (->> "adventofcode/year2018/day06/input.txt" io/resource slurp (region-size 10000)))