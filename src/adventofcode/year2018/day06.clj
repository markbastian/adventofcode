(ns adventofcode.year2018.day06
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.data :refer [diff]]
            [clojure.pprint :as pp]))

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

(defn manhattan-dist [[a b] [c d]]
  (+ (Math/abs ^int (- a c))
     (Math/abs ^int (- b d))))

(defn assign-group [grp]
  (let [[a b] (distinct (map second grp))]
    (if b '. a)))

(defn step1 [{:keys [frontier visited]}]
  (let [new-visited (into visited frontier)
        new-frontier (for [[k v] frontier n (neighbors k)
                           :when (not (new-visited n))] [n v])]
    {:frontier (->> new-frontier
                    (group-by first)
                    (map (fn [[k v]] [k (assign-group v)])))
     :visited  new-visited}))

(defn group-frequencies [v] (frequencies (map second v)))

;TODO - Better understand the stopping criteria.
(comment
  (->> {:frontier (zipmap (->> "adventofcode/year2018/day06/input.txt"
                               io/resource
                               slurp
                               input->pts)
                          (range))
        :visited  {}}
       (iterate step1)
       (map :visited)
       (drop 55)
       (map group-frequencies)
       (take 1)
       (map (fn [m] (get m 4)))
       ;first
       ;(map second)
       ;frequencies
       ))

(comment
  (solution input)
  ;2906
  (->> "adventofcode/year2018/day06/input.txt" io/resource slurp solution))

(defn roi-step [{:keys [frontier visited]}]
  (let [new-visited (into visited frontier)
        new-frontier (distinct (remove new-visited (mapcat neighbors frontier)))]
    {:frontier new-frontier
     :visited  new-visited}))

(defn centroid [pts]
  (->> pts
       (reduce (fn [a b] (map + a b)))
       (mapv #(quot % (count pts)))))

(defn total-dist [c pts]
  (reduce + (for [p pts] (manhattan-dist p c))))

(defn filter-frontier [pts tol frontier]
  (->> frontier
       (map (fn [p] (total-dist p pts)))
       (filter (fn [d] (< d tol)))))

(defn region-size [tol input]
  (let [pts (input->pts input)]
    (->> {:frontier [(centroid pts)] :visited #{}}
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