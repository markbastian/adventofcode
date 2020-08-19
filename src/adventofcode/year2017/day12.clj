(ns adventofcode.year2017.day12
  (:require [clojure.string :as cs]
            [clojure.java.io :as io])
  (:import (clojure.lang PersistentQueue)))

(def empty-queue PersistentQueue/EMPTY)

(defn parse-line [line]
  (let [[f t] (cs/split line #"<->")
        tv (map cs/trim (cs/split t #","))]
    [(cs/trim f) tv]))

(defn input->neighbors [input]
  (->> input cs/split-lines (map parse-line) (into {})))

(def example-input
  (input->neighbors "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5\n"))

(def input (-> "adventofcode/year2017/day12/input.txt"
               io/resource
               slurp
               input->neighbors))

(defn step [{:keys [frontier neighbors-fn visited] :as m}]
  (when-some [current-pos (peek frontier)]
    (let [new-neighbors (remove (partial contains? visited) (neighbors-fn current-pos))]
      (-> m
          (update :frontier (fn [f] (into (pop f) new-neighbors)))
          (update :visited into (zipmap new-neighbors (repeat current-pos)))))))

(defn bfs [start input-map]
  (->> {:frontier     (conj empty-queue start)
        :neighbors-fn input-map
        :visited      {start nil}}
       (iterate step)
       (take-while identity)
       (map :visited)))

(defn reachable-group [start input-map]
  (->> (bfs start input-map) last keys set))

(comment
  (= 6 (count (reachable-group "0" example-input)))
  (= 175 (count (reachable-group "0" input))))

(defn groups [input-map]
  (loop [m input-map res []]
    (if-some [k (ffirst m)]
      (let [group (reachable-group k m)]
        (recur (apply dissoc m group) (conj res group)))
      res)))

(comment
  (= 6 (count (reachable-group "0" example-input)))
  (= 175 (count (reachable-group "0" input)))

  (= 2 (count (groups example-input)))
  (= 213 (count (groups input))))