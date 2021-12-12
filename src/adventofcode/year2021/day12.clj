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
  (if-some [[path visited] (peek incomplete-paths)]
    (letfn [(path-state [[path]] (if (= "end" (peek path)) :complete :incomplete))]
      (let [paths (->> (neighbors-fn (peek path))
                       (filter (partial filter-fn visited))
                       (map (fn [neighbor]
                              [(conj path neighbor)
                               (cond-> visited
                                       (re-matches #"[a-z]+" neighbor)
                                       (update neighbor (fnil inc 0)))]))
                       (group-by path-state))]
        (-> state
            (update :incomplete-paths (fn [p] (into (pop p) (:incomplete paths))))
            (update :complete-paths into (map first (:complete paths))))))
    state))

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