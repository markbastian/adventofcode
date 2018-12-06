(ns adventofcode.year2018.day06
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

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

(defn step [[[minx maxx] [miny maxy]] m]
  (let [n (distinct (for [[k v] m [i j :as n] (neighbors k)
                          :when (not (m n))
                          ;:when (and (<= minx i maxx) (<= miny j maxy))
                          ]
                      [n v]))]
    (conj
      (into {} n)
      (zipmap (overlaps n) (repeat nil))
      m)))

(defn finites [[a b]]
  (letfn [(f [grp] (into {} (map (fn [[g v]] [g (set (map first v))]) (group-by second grp))))]
    (merge-with (fn [a b] (when (= a b) a)) (f a) (f b))))

(defn solution [input]
  (let [p (->> input (format "[%s]") edn/read-string (partition 2) (mapv vec))
        [xs ys] (apply mapv vector p)
        minx (apply min xs)
        maxx (apply max xs)
        miny (apply min ys)
        maxy (apply max ys)
        g (vec (repeat (inc (- maxx minx)) (vec (repeat (inc (- maxy miny)) nil))))
        res (->> (zipmap p (range))
                 (iterate (partial step [[minx maxx] [miny maxy]]))
                 (partition 2 1)
                 (drop 50)
                 first
                 finites
                 ;(drop-while (fn [[a b]] (not= a b)))
                 ;ffirst
                 ;(drop 20)
                 ;first
                 ;(group-by second)
                 ;(map (fn [[k v]] [k (count (map first v))]))
                 )]
    res #_(apply mapv vector (reduce (fn [m [k v]] (assoc-in m (mapv - k [minx miny]) (or v '.))) g res))))

;(solution input)
;(->> "adventofcode/year2018/day06/input.txt" io/resource slurp solution)
