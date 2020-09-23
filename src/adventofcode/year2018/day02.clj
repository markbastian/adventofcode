(ns adventofcode.year2018.day02
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(defn checksum [input]
  (->> input
       cs/split-lines
       (mapcat (comp #(remove #{1} %) distinct vals frequencies))
       frequencies
       vals
       (apply *)))

(defn similars [a b]
  (let [x (cs/join (map (fn [a b] (when (= a b) a)) a b))]
    (when (= (dec (count a)) (count x)) x)))

(defn one-off [input]
  (loop [[f & r] (cs/split-lines input)]
    (when f
      (if-some [s (some #(similars f %) r)]
        s
        (recur r)))))

(def test-input-1 "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab")
(def test-input-2 "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz")
(def input (-> "adventofcode/year2018/day02/input.txt" io/resource slurp))

(comment
  ;Part 1
  (time (= 12 (checksum test-input-1)))
  (time (= 9633 (checksum input)))
  ;Part 2
  (time (= "fgij" (one-off test-input-2)))
  (time (= "lujnogabetpmsydyfcovzixaw" (one-off input))))