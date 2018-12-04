(ns adventofcode.year2018.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def input
  "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")

()

(def shift-rgx #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})] Guard #(\d+) begins shift")
(def sleep-rgx #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})] falls asleep")
(def wake-rgx #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})] wakes up")
(def sleep-wake-rgx #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})]\s+(.*)")

(defn parse-lines [input]
  (loop [[f & r] input grps []]
    (if f
      (let [[_ date hr mm gno] (re-matches shift-rgx f)
            [b a] (split-with (fn [s]
                                (or
                                  (cs/includes? s "wakes up")
                                  (cs/includes? s "falls asleep"))) r)
            x (map (comp
                     (fn [[_ date hr mm aw]] [date (Integer/parseInt hr) (Integer/parseInt mm) aw])
                     (partial re-matches sleep-wake-rgx)) b)]
        (recur a (conj grps
                       [(Integer/parseInt gno)
                        (cons [date (Integer/parseInt hr) (Integer/parseInt mm)] x)])))
      grps)))

(parse-lines (cs/split-lines input))

(->> "adventofcode/year2018/day04/input.txt"
     io/resource
     slurp
     cs/split-lines
     sort)