(ns adventofcode.year2018.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.edn :as edn]
            [clojure.set :as s]))

(defn addr [v va vb c] (assoc v c (+ (get v va) (get v vb))))
(defn addi [v va vb c] (assoc v c (+ (get v va) vb)))
(defn mulr [v va vb c] (assoc v c (* (get v va) (get v vb))))
(defn muli [v va vb c] (assoc v c (* (get v va) vb)))
(defn banr [v va vb c] (assoc v c (bit-and (get v va) (get v vb))))
(defn bani [v va vb c] (assoc v c (bit-and (get v va) vb)))
(defn borr [v va vb c] (assoc v c (bit-or (get v va) (get v vb))))
(defn bori [v va vb c] (assoc v c (bit-or (get v va) vb)))
(defn setr [v va _ c] (assoc v c (get v va)))
(defn seti [v va _ c] (assoc v c va))
(defn gtir [v va vb c] (assoc v c (if (> va (get v vb)) 1 0)))
(defn gtri [v va vb c] (assoc v c (if (> (get v va) vb) 1 0)))
(defn gtrr [v va vb c] (assoc v c (if (> (get v va) (get v vb)) 1 0)))
(defn eqir [v va vb c] (assoc v c (if (= va (get v vb)) 1 0)))
(defn eqri [v va vb c] (assoc v c (if (= (get v va) vb) 1 0)))
(defn eqrr [v va vb c] (assoc v c (if (= (get v va) (get v vb)) 1 0)))

(def ops
  [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

(defn num-ops [before [_ a b c] after]
  (count (filter #(= after (% before a b c)) ops)))

(comment
  ;3
  (num-ops [3 2 1 1] [9 2 1 2] [3 2 2 1]))

(def input (->> "adventofcode/year2018/day16/input.txt" io/resource slurp))

(comment
  ;547
  (->> input
       (re-seq #"Before: (\[\d, \d, \d, \d\])\n(\d+ \d \d \d)\nAfter:  (\[\d, \d, \d, \d\])")
       (map (fn [[_ a b c]] [a (str "[" b "]") c]))
       (map (fn [v] (mapv edn/read-string v)))
       (map (fn [v] (apply num-ops v)))
       (filter (fn [v] (> v 2)))
       count))

(defn nops [before [n a b c] after]
  [n (->> ops
          (filter #(= after (% before a b c)))
          set)])

(def potentials
  (->> input
       (re-seq #"Before: (\[\d, \d, \d, \d\])\n(\d+ \d \d \d)\nAfter:  (\[\d, \d, \d, \d\])")
       (map (fn [[_ a b c]] [a (str "[" b "]") c]))
       (map (fn [v] (mapv edn/read-string v)))
       (map (fn [v] (apply nops v)))
       (group-by first)
       (map (fn [[k v]] [k (apply s/intersection (map second v))]))))

(defn simplify [[potentials actuals]]
  (let [[k s :as lock]
        (some (fn [[_ v :as soln]] (when (= 1 (count v)) soln)) potentials)
        x (first s)]
    [(->> potentials
          (remove #{lock})
          (map (fn [[k v]] [k (disj v x)])))
     (assoc actuals k x)]))

(def mops
  (->> (iterate simplify [potentials])
       (drop-while (comp seq first))
       first
       second))

(def input2
  (loop [[f & r] (->> input cs/split-lines) res []]
    (cond
      (nil? f) (map #(edn/read-string (str "[" % "]")) res)
      (re-matches #"\d+ \d \d \d" f) (recur r (conj res f))
      :else (recur r []))))

(defn do-op [[op a b c :as v]] ((mops op) v a b c))

(comment
  ;582 - [582 6 582 2]
  (reduce
    (fn [v [op a b c]] ((mops op) v a b c))
    input2))

;(nops [3 2 1 1] [9 2 1 2] [3 2 2 1])