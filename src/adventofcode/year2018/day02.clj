(ns adventofcode.year2018.day02
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def test-input-1 "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab")
(def test-input-2 "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz")
(def input (-> "adventofcode/year2018/day02/input.txt" io/resource slurp))

(defn checksum [input]
  (->> input
       cs/split-lines
       (mapcat (comp #(remove #{1} %) distinct vals frequencies))
       frequencies
       vals
       (apply *)))

(comment
  (time (= 12 (checksum test-input-1)))
  (time (= 9633 (checksum input))))

(defn similars [a b]
  (let [x (cs/join (map (fn [a b] (when (= a b) a)) a b))]
    (when (= (dec (count a)) (count x)) x)))

(defn one-off [input]
  (loop [[f & r] (cs/split-lines input)]
    (if-some [s (some #(similars f %) r)]
      s
      (recur r))))

(comment
  (time (= "fgij" (one-off test-input-2)))
  (time (= "lujnogabetpmsydyfcovzixaw" (one-off input))))