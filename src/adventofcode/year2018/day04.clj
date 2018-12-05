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

(def shift-rgx #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})] Guard #(\d+) begins shift")
(def sleep-wake-rgx #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})]\s+(.*)")

(defn parse-lines [input]
  (loop [[f & r] input grps []]
    (if f
      (let [[_ _ _ _ gno] (re-matches shift-rgx f)
            [b a] (split-with (fn [s]
                                (or
                                  (cs/includes? s "wakes up")
                                  (cs/includes? s "falls asleep"))) r)
            x (map (comp
                     (fn [[_ date hr mm aw]] [date (Integer/parseInt hr) (Integer/parseInt mm) aw])
                     (partial re-matches sleep-wake-rgx)) b)]
        (recur a (conj grps
                       [(Integer/parseInt gno)
                        (->> x
                             (partition 2)
                             (map (fn [[[_ _ s] [_ _ f]]] (range s f))))])))
      (->> grps
           (group-by first)
           (map (fn [[g s]] [g (flatten (map second s))]))))))

(defn parse-input [input]
  (->> input
       cs/split-lines
       sort
       parse-lines))

(defn guard-score [[n s]]
  (* n (->> s frequencies (apply max-key second) first)))

(defn solution [input]
  (->> input parse-input (apply max-key (comp count second)) guard-score))

(comment
  ;240
  (solution input)
  ;101262
  (->> "adventofcode/year2018/day04/input.txt" io/resource slurp solution))

(defn solution2 [input]
  (->> input
       parse-input
       (map (fn [[g s]] [g (frequencies s)]))
       (filter (comp seq second))
       (map (fn [[g f]] (cons g (apply max-key second f))))
       (apply max-key last)
       butlast
       (apply *)))

(comment
  ;4455
  (solution2 input)
  ;71976
  (->> "adventofcode/year2018/day04/input.txt" io/resource slurp solution2))


