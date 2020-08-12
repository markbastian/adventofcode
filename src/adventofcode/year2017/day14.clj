(ns adventofcode.year2017.day14
  (:require [adventofcode.year2017.day10 :as day10]
            [adventofcode.year2017.day12 :as day12]
            [clojure.string :as cs]))

(defn hex-digit->binary-str [s]
  (let [b (Long/toBinaryString (Long/parseLong (str s) 16))]
    (format "%04d" (Long/parseLong b))))

(defn knot-hash->binary-string [s]
  (->> s day10/dense-hash (map hex-digit->binary-str) cs/join))

(defn disk-grid [hash]
  (for [i (range 128)
        :let [k (str hash "-" i)]]
    (knot-hash->binary-string k)))

(defn used-squares [hash]
  (let [g (disk-grid hash)]
    ((frequencies (cs/join g)) \1)))

(comment
  (= 8108 (used-squares "flqrgnkx"))
  (= 8230 (used-squares "hfdlxzhv")))

(defn neighbors [[i j]]
  (let [u ((juxt inc identity dec identity) i)
        v ((juxt identity inc identity dec) j)]
    (map vector u v)))

(defn create-neighbors [hash-str]
  (let [g (mapv vec (disk-grid hash-str))]
    (into {}
          (for [i (range (count g))
                j (range (count (g i)))
                :let [coord [i j]]
                :when (= \1 (get-in g coord))]
            [coord (filter (fn [n] (= \1 (get-in g n))) (neighbors coord))]))))

(comment
  (= 1242 (count (day12/groups (create-neighbors "flqrgnkx"))))
  (= 1103 (count (day12/groups (create-neighbors "hfdlxzhv")))))
