(ns adventofcode.year2021.day12
  (:require [adventofcode.input-util :as iu]
            [clojure.string :as str]))

(defn parse-input [input]
  (->> (iu/read-as-strings input)
       (map (fn [line] (str/split line #"-")))
       (reduce (fn [m [a b]]
                 (cond-> m
                         (and (not= a "end") (not= b "start"))
                         (update a (comp set conj) b)
                         (and (not= a "start") (not= b "end"))
                         (update b (comp set conj) a)))
               {})))

(def sample-input (parse-input "adventofcode/year2021/day12/sample-input.txt"))
(def input (parse-input "adventofcode/year2021/day12/input.txt"))

(defn step [{:keys [incomplete-paths neighbors-fn filter-fn] :as state}]
  (let [new-paths (for [[path visited] incomplete-paths
                        :let [current (peek path)]
                        neighbor (neighbors-fn current)
                        :when (filter-fn visited neighbor)]
                    [(conj path neighbor)
                     (cond-> visited
                             (re-matches #"[a-z]+" neighbor)
                             (update neighbor (fnil inc 0)))])
        {complete true incomplete false} (group-by (fn [[path]] (= "end" (peek path))) new-paths)]
    (-> state
        (assoc :incomplete-paths incomplete)
        (update :complete-paths into (map first complete)))))

(defn cave-paths [input filter-fn]
  (->> {:incomplete-paths [[["start"] {}]]
        :complete-paths   []
        :neighbors-fn     input
        :filter-fn        filter-fn}
       (iterate step)
       (drop-while (comp seq :incomplete-paths))
       first
       :complete-paths
       count))

(defn visit-once [visited neighbor]
  (not (visited neighbor)))

(defn visit-once-plus-one [visited neighbor]
  (or
    (visit-once visited neighbor)
    (every? #{1} (vals visited))))

(comment
  (= 10 (cave-paths sample-input visit-once))
  (= 4378 (cave-paths input visit-once))
  (= 36 (cave-paths sample-input visit-once-plus-one))
  (= 133621 (cave-paths input visit-once-plus-one))
  )