(ns adventofcode.year2020.day07
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(defn parse-line [s]
  (let [[f & r] (cs/split (cs/replace s #"bags?" "") #"\s*contain\s*")
        [m c] (cs/split f #"\s+")]
    [{:color c :modifier m} r]))

(defn read-input [s]
  (->> (io/resource s)
       slurp
       cs/split-lines
       (map parse-line)))

(let [s "light red bags contain 1 bright white bag, 2 muted yellow bags."
      [f r] (cs/split (cs/replace s #"bags?" "") #"\s*contain\s*")
      [m c] (cs/split f #"\s+")
      r (->> (cs/split (cs/replace r #"\s*\." "") #"\s*,\s*")
             (map (fn [s]
                    (let [[n m c] (cs/split s #"\s+")]
                      {:color c :modifier m :count (Long/parseLong n)}))))]
  [{:color c :modifier m} r])

(def test-input (read-input "adventofcode/year2020/day07/test-input.txt"))
(def input (read-input "adventofcode/year2020/day07/input.txt"))