(ns adventofcode.year2018.day08
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

;2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
;A----------------------------------
;    B----------- C-----------
;                     D-----

(def input "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

(defn parse-input [s]
  (edn/read-string (str "[" s "]")))

(parse-input input)

(defn compute-meta [input]
  (loop [[nc nm & r] input [[a b] & ops :as stack] nil sums []]
    (cond
      (nil? nc) sums
      (zero? nc) (let [[meta r] (split-at nm r)
                       r (cond->> r b (cons b) a (cons (dec a)))]
                   (recur r ops (conj sums meta)))
      :else (recur r (conj stack [nc nm]) sums))))

(defn meta-sum [input]
  (->> input compute-meta flatten (reduce +)))

(compute-meta (parse-input input))

(comment
  (meta-sum (parse-input input))
  (->> "adventofcode/year2018/day08/input.txt" io/resource slurp parse-input meta-sum))

(defn total-meta [[ids :as v]]
  (cond
    (nil? ids) 0
    (seq ids) (reduce + (map #(total-meta (get v %)) ids))
    :else (reduce + (flatten v))))

(let [input (parse-input input)]
  (loop [[num-children num-meta & r :as s] input
         [[remaining-children pushed-meta] & ops :as stack] []
         sums []]
    (prn {:s s :stack stack :sum sums})
    (cond
      (nil? num-children) sums

      (some-> remaining-children zero?)
      (let [c (peek sums)
            x (pop sums)
            ops (if-some [[a b] (peek ops)] (conj (pop ops) [(dec a) b]) ops)
            [meta r] (split-at pushed-meta s)]
        (recur
          r
          ops
          (conj x (into [(vec meta)] c))))

      (zero? num-children)
      (let [[meta r] (split-at num-meta r)
            c (peek sums)
            x (pop sums)]
        (recur
          r
          (conj ops [(dec remaining-children) pushed-meta])
          (conj x (conj c [[] (vec meta)]))))

      :else (recur r (conj stack [num-children num-meta]) (conj sums [])))))

(def res
  [[1 1 2]
   [[] [10 11 12]]
   [[2]
    [[] [99]]]])

(defn doit [[nc nm & r]]
  (if (zero? nc)
    (let [[meta & r] (split-at nm r)]
      [[[] meta] r])
    (let [[r] (take nm (doit r))]
      [meta c])))