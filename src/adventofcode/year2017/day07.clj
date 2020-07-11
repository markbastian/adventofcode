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

(comment
  (build-map input)
  (build-map test-input))

(defn root [input]
  (let [m (build-map input)]
    (first (s/difference (set (vals m)) (set (keys m))))))

;Part 1
(comment
  (root test-input)
  (root input))

(def schema
  {:name     {:db/unique :db.unique/identity}
   :children {:db/cardinality :db.cardinality/many
              :db/valueType   :db.type/ref
              :db/isComponent true}})

(defn build-data [input]
  (->> input
       cs/split-lines
       (map (partial re-matches #"([a-z]+)\s+\((\d+)\)(.*)"))
       (map (fn [[_ s w r]]
              (let [c (-> r (cs/replace " -> " "") (cs/replace #"\s+" "") (cs/split #","))
                    children (map (fn [n] {:name n}) (filter seq c))]
                (cond-> {:name s :weight (edn/read-string w)}
                        (seq children)
                        (assoc :children children)))))))

(comment
  (build-data input))

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

(comment
  ;Solve part 1 using datascript
  (->> "adventofcode/year2017/day07/input.txt" io/resource slurp build-db root-ds))

(defn build-tree [db]
  (d/pull db '[:children :name :weight] [:name (root-ds db)]))

(defn compute-total-weights [{:keys [children weight] :as m}]
  (if children
    (let [c (map compute-total-weights children)
          ib (mapcat :imbalances c)
          w (map :total-weight c)
          balanced? (apply = w)]
      (-> m
          (assoc :children c)
          (assoc :total-weight (reduce + weight w))
          (assoc :imbalances (cond-> ib (not balanced?) (conj [w (map :weight c)])))))
    (assoc m :total-weight weight)))

(comment
  (compute-total-weights (build-tree (build-db input)))
  (compute-total-weights (build-tree (build-db test-input))))

(comment
  ;Experimental to see how much can be solved with just datalog
  (defn weights [input]
    (d/q
      '[:find ?n ?w (sum ?cw)
        :in $
        :with ?c
        :where
        [?e :weight ?w]
        [?e :name ?n]
        [?e :children ?c]
        [?c :weight ?cw]]
      (build-db input)))

  (->> (d/q
         '[:find ?pn ?n ?s
           :in $ [[?n ?w ?tw]]
           :with ?c
           :where
           [(+ ?w ?tw) ?s]
           [?e :name ?pn]
           [?e :children ?c]
           [?c :name ?n]]
         (build-db input) (weights input))
       (group-by first)
       (map (fn [[k v]] [k (map rest v)]))))

(defn find-imbalance [[tw w]]
  (let [m (frequencies tw)
        z (zipmap (vals m) (keys m))
        outlier (z 1)
        norm (first (vals (dissoc z 1)))
        fix (- norm outlier)]
    (->> (map vector tw w)
         (some (fn [[a b]] (when (= a outlier) (+ b fix)))))))

(defn part2 [input]
  (->> input
       build-db
       build-tree
       compute-total-weights
       :imbalances
       last
       find-imbalance))

(comment
  ;Part 2
  (part2 input)
  (part2 test-input)
  )