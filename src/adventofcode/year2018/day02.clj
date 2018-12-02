(ns adventofcode.year2018.day02
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def input
  "abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab")

(defn checksum [input]
  (->> input
       cs/split-lines
       (map cs/trim)
       (mapcat (comp #(remove #{1} %) distinct vals frequencies))
       frequencies
       vals
       (apply *)))

(comment
  (->> "adventofcode/year2018/day02/input.txt" io/resource slurp checksum))

(def input2 "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz")

(defn similars [a b]
  (let [x (cs/join (map (fn [a b] (when (= a b) a)) a b))]
    (when (= (dec (count a)) (count x)) x)))

(defn one-off [input]
  (loop [[f & r] (->> input cs/split-lines)]
    (if-some [s (some #(similars f %) r)]
      s
      (recur r))))


(comment
  ;"lujnogabetpmsydyfcovzixaw"
  (->> "adventofcode/year2018/day02/input.txt"
       io/resource
       slurp
       one-off))