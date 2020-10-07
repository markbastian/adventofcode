(ns adventofcode.year2018.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def sample-input (->> "adventofcode/year2018/day04/sample-input.txt" io/resource slurp))
(def input (->> "adventofcode/year2018/day04/input.txt" io/resource slurp))

(def shift-rgx #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})] Guard #(\d+) begins shift")
(def sleep-wake-rgx #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})]\s+(.*)")

(defn parse-int [s] (Integer/parseInt s))

(defn wake-or-sleep-even? [s]
  (or (cs/includes? s "wakes up") (cs/includes? s "falls asleep")))

(defn parse-sleep-wake-line [s]
  (let [[_ date hr mm aw] (re-matches sleep-wake-rgx s)]
    [date (parse-int hr) (parse-int mm) aw]))

(defn parse-lines [input]
  (loop [[f & r] input groups []]
    (if f
      (let [[_ _ _ _ guard-number] (re-matches shift-rgx f)
            [shift-events post] (split-with wake-or-sleep-even? r)
            parsed (map parse-sleep-wake-line shift-events)]
        (recur post (conj groups
                          [(parse-int guard-number)
                           (->> (partition 2 parsed)
                                (map (fn [[[_ _ s] [_ _ f]]] (range s f))))])))
      (letfn [(f [m [g minutes]] (update m g (fn [v] (into v (flatten minutes)))))]
        (reduce f {} groups)))))

(defn parse-input [input] (->> input cs/split-lines sort parse-lines))

(comment
  (parse-input sample-input)
  (parse-input input))

(defn guard-score [[n s]]
  (* n (->> s frequencies (apply max-key second) first)))

(defn solution [input]
  (->> input parse-input (apply max-key (comp count second)) guard-score))

(comment
  (time (= 240 (solution sample-input)))
  (time (= 101262 (solution input))))

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
  (time (= 4455 (solution2 sample-input)))
  (time (= 71976 (solution2 input))))


