(ns adventofcode.year2018.day08
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.core.async :as async]))

;2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
;A----------------------------------
;    B----------- C-----------
;                     D-----

(def input "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

(defn parse-input [s]
  (edn/read-string (str "[" s "]")))

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

(comment
  (meta-sum (parse-input input))
  (->> "adventofcode/year2018/day08/input.txt" io/resource slurp parse-input meta-sum))

(defn parse-tree [c]
  (let [nc (async/<!! c) nm (async/<!! c)]
    {:children (vec (repeatedly nc #(parse-tree c)))
     :meta     (vec (repeatedly nm #(async/<!! c)))}))

(defn node-seq [{:keys [children meta]}]
  (if (seq children)
    (mapcat (comp node-seq #(get children %) dec) meta)
    meta))

(defn part2 [m] (reduce + (node-seq m)))

(comment
  (->> input parse-input async/to-chan parse-tree part2)
  (->> "adventofcode/year2018/day08/input.txt"
       io/resource
       slurp
       parse-input
       async/to-chan
       parse-tree
       part2))