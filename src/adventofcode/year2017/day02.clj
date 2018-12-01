(ns adventofcode.year2017.day02
  (:require [clojure.edn :as edn]
            [clojure.string :as cs]
            [clojure.java.io :as io]))

(def input "5 1 9 5\n7 5 3\n2 4 6 8")

(defn checksum [s]
  (letfn [(f [s] (map edn/read-string s))]
    (let [mm (juxt (partial apply max) (partial apply min))]
      (->> s
           cs/split-lines
           (map (comp #(apply - %) mm f (partial re-seq #"\d+")))
           (reduce +)))))

(comment
  ;18
  (checksum input)
  ;21845
  (-> "adventofcode/year2017/day02/input.txt" io/resource slurp checksum))

(def input2
  "5 9 2 8
   9 4 7 3
   3 8 6 5")

(defn divisor [s]
  (loop [[den & nums] (sort s)]
    (if-some [v (some (fn [num] (when (zero? (mod num den)) (/ num den))) nums)]
      v
      (recur nums))))

(defn checksum2 [s]
  (letfn [(f [s] (map edn/read-string s))]
    (->> s
         cs/split-lines
         (map (comp divisor f (partial re-seq #"\d+")))
         (reduce +))))

(comment
  ;9
  (checksum2 input2)
  ;191
  (-> "adventofcode/year2017/day02/input.txt" io/resource slurp checksum2))