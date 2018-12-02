(ns adventofcode.year2017.day07
  (:require [clojure.string :as cs]
            [clojure.set :as s]
            [clojure.java.io :as io]
            [datascript.core :as d]
            [datascript.db :as db]
            [clojure.edn :as edn]))

;https://adventofcode.com/2017/day/7#part2

(def input
  "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)")

(defonce test-input (->> "adventofcode/year2017/day07/input.txt" io/resource slurp))

(defn build-map [input]
  (->> input
       cs/split-lines
       (map (partial re-matches #"([a-z]+)\s+\((\d+)\)(.*)"))
       (map (fn [[_ s w r]]
              (let [dsts (-> r
                             (cs/replace " -> " "")
                             (cs/replace #"\s+" "")
                             (cs/split #","))]
                (reduce (fn [m dst] (assoc m dst s)) {} (filter seq dsts)))))
       (reduce into)))

(defn root [input]
  (let [m (build-map input)]
    (first (s/difference (set (vals m)) (set (keys m))))))

;Part 1
(comment
  (root test-input)
  (root input))

(defn build-data [input]
  (->> input
       cs/split-lines
       (map (partial re-matches #"([a-z]+)\s+\((\d+)\)(.*)"))
       (map (fn [[_ s w r]]
              (let [c (-> r (cs/replace " -> " "") (cs/replace #"\s+" "") (cs/split #","))]
                {:name     s
                 :weight   (edn/read-string w)
                 :children (map (fn [n] {:name n}) (filter seq c))})))))

(def schema
  {:name     {:db/unique :db.unique/identity}
   :children {:db/cardinality :db.cardinality/many
              :db/valueType   :db.type/ref
              :db/isComponent true}})

(defn build-db [input]
  (d/db-with (db/empty-db schema) (build-data input)))

;Problem 1
(defn root-ds [db]
  (d/q
    '[:find ?n .
      :in $
      :where
      [?parent :children ?e]
      [(missing? $ ?parent :_children)]
      [?parent :name ?n]]
    db))

(->> "adventofcode/year2017/day07/input.txt" io/resource slurp build-db root-ds)

(group-by first
          (d/q
  '[:find ?n ?cn ?w
    :in $
    :where
    [?parent :children ?child]
    [?parent :name ?n]
    [?child :weight ?w]
    [?child :name ?cn]]
  (build-db input)))

(defn build-tree [db]
  (d/pull db '[*] [:name (root-ds db)]))

(defn balance-tree [{:keys [name weight children]}]
  (if children
    (let [weights (map balance-tree children)]
      (if (apply = weights)
        (+ weight (reduce + weights))
        (do
          (prn (->> (map vector weights (map :weight children))
                    (group-by first)
                    (map (fn [[k v]] [k (map second v)]))))
          (+ weight (reduce + weights)))))
    weight))

;While solved, this one feels a bit less than satisfactory :(
;Need a single function that I like.
; Use the output of this to visually determine the result
;(foo (build-tree (build-db test-input)))