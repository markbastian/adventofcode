(ns adventofcode.year2018.day07
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def input
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(def rgx #"Step ([A-Z]) must be finished before step ([A-Z]) can begin\.")

(defn step [{:keys [steps order]}]
  (let [blockers (set (map second steps))
        [[f to]] (->> steps (remove (fn [[f]] (blockers f))) (sort-by first))
        remaining-steps (set (remove (fn [[a]] (= a f)) steps))]
    {:steps (seq (cond-> remaining-steps to (conj [to])))
     :order (str order f)}))

(defn parse [input]
  (->> input
       (re-seq rgx)
       (map (fn [[_ f t]] [f t]))))

(defn order [input]
  (->> {:steps (parse input) :order ""}
       (iterate step)
       (drop-while :steps)
       first
       :order))

(comment
  ;"CABDFE"
  (order input)
  ;"AHJDBEMNFQUPVXGCTYLWZKSROI"
  (->> "adventofcode/year2018/day07/input.txt" io/resource slurp order))

(def durations
  (zipmap
    (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (map #(inc (+ 60 %)) (range))))

(defn work-step [{:keys [jobs]}])