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

(defn input->pts [input]
  (->> input (format "[%s]") edn/read-string (partition 2) (mapv vec)))

(defn manhattan-dist [[a b] [c d]]
  (+ (Math/abs ^int (- a c)) (Math/abs ^int (- b d))))

(defn bounds [input]
  (let [x (map first input) y (map second input)]
    [[(apply min x) (apply max x)]
     [(apply min y) (apply max y)]]))

(defn dist-data [pts]
  (let [[[minx maxx] [miny maxy]] (bounds pts)]
    (for [i (range minx (inc maxx)) j (range miny (inc maxy)) :let [dst [i j]] p pts]
      {:src p :dst dst :dist (manhattan-dist p dst)})))

(defn part1 [pts]
  (let [[x y] (bounds pts)]
    (->> pts
         dist-data
         (group-by :dst)
         (map (fn [[_ grp]] (->> grp (sort-by :dist) (partition-by :dist) first)))
         (filter (fn [grp] (= 1 (count grp))))
         flatten
         (group-by :src)
         (reduce (fn [m [k v]] (assoc m k (mapv :dst v))) {})
         (remove
           (fn [[_ grp]]
             (let [[xs ys] (apply map vector grp)]
               (or (some (set x) xs) (some (set y) ys)))))
         (map (fn [[pt pts]] [pt (count pts)]))
         (apply max-key second))))

(defn part2 [sz pts]
  (->> pts
       dist-data
       (group-by :dst)
       (map (fn [[_ v]] (reduce + (map :dist v))))
       (filter #(< % sz))
       count))

(comment
  (->> input input->pts part1)
  ;2906
  (->> "adventofcode/year2018/day06/input.txt" io/resource slurp input->pts part1))

(comment
  ;16
  (->> input input->pts (part2 32))
  ;50530
  (->> "adventofcode/year2018/day06/input.txt" io/resource slurp input->pts (part2 10000)))