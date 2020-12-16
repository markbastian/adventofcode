(ns adventofcode.year2020.day07
  (:require [clojure.string :as cs]
            [clojure.java.io :as io])
  (:import (clojure.lang PersistentQueue)))

(defn parse-contents [s]
  (when-not (cs/includes? s "no other")
    (->> (cs/split (cs/replace s #"\s*\." "") #"\s*,\s*")
         (map (fn [s]
                (when-not (= "no other" s)
                  (let [[n m c] (cs/split s #"\s+")]
                    {:color c :modifier m :count (Long/parseLong n)})))))))

(defn parse-line [s]
  (let [[f r] (cs/split (cs/replace s #"bags?" "") #"\s*contain\s*")
        [m c] (cs/split f #"\s+")]
    [{:color c :modifier m} (parse-contents r)]))

(defn read-input [s]
  (->> (io/resource s)
       slurp
       cs/split-lines
       (map parse-line)
       (into {})))

(def test-input (read-input "adventofcode/year2020/day07/test-input.txt"))
(def input (read-input "adventofcode/year2020/day07/input.txt"))

(defn neighbors [input]
  (reduce
    (fn [m [o i]] (update m i (comp set conj) o))
    {}
    (for [[outer vs] input v vs] [outer (dissoc v :count)])))

(defn visit [input]
  (let [neighbors-fn (neighbors input)]
    (loop [frontier (conj PersistentQueue/EMPTY {:color "gold", :modifier "shiny"})
           visited #{}]
      (if-some [n (peek frontier)]
        (let [r (pop frontier)
              new-neighbors (remove visited (neighbors-fn n))]
          (recur (into r new-neighbors) (conj visited n)))
        visited))))

(comment
  (time (= 4 (dec (count (visit test-input)))))
  (time (= 151 (dec (count (visit input))))))

(defn dfs [bag input]
  (if-some [bags (input bag)]
    (->> bags
         (map (fn [{:keys [count] :as bag}]
                (* count (dfs (dissoc bag :count) input))))
         (reduce +)
         inc)
    1))

(comment
  (time (= 32 (dec (dfs {:color "gold", :modifier "shiny"} test-input))))
  (time (= 41559 (dec (dfs {:color "gold", :modifier "shiny"} input)))))