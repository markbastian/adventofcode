(ns adventofcode.year2020.day07
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

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

(reduce
  (fn [m [k vs]]
    (reduce (fn [m v] (assoc m v k)) m vs))
  {}
  test-input)