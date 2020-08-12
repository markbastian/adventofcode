(ns adventofcode.year2017.day10
  (:require [clojure.string :as cs]
            [clojure.test :refer :all]))

(def input [227 169 3 166 246 201 0 47 1 255 2 254 96 3 97 144])
(def benchmark-input [120 93 0 90 5 80 129 74 1 165 204 255 254 2 50 113])

(defn swap-pos [v a b]
  (let [c (count v) a (mod a c) b (mod b c)]
    (assoc v a (v b) b (v a))))

(defn rev-len [v from len]
  (letfn [(rev-block [v from to]
            (if (>= from to) v (recur (swap-pos v from to) (inc from) (dec to))))]
    (rev-block v from (dec (+ from len)))))

(defn knot-hash-run-step [[v pos skip] len]
  [(rev-len v pos len) (mod (+ pos len skip) (count v)) (inc skip)])

(defn knot-hash-run
  ([v lengths pos skip]
   (reductions knot-hash-run-step [v pos skip] lengths))
  ([v lengths]
   (knot-hash-run v lengths 0 0)))

(def knot-hash-round (comp last knot-hash-run))

(comment
  (reductions
    knot-hash-run-step
    [[0 1 2 3 4] 0 0]
    [3 4 1 5])

  (knot-hash-run [0 1 2 3 4] [3 4 1 5])
  (knot-hash-round [0 1 2 3 4] [3 4 1 5]))

(defn sparse-hash [v lens]
  (->> (knot-hash-round v lens) first (take 2) (apply *)))

(def v256 (vec (range 256)))

(deftest sparse-hash-test
  (testing "Ensure sparse-hash is producing the correct value based on the example"
    (is (= 12 (sparse-hash [0 1 2 3 4] [3 4 1 5])))))

(comment
  ;Part 1
  (time (= 13760 (sparse-hash v256 input)))
  (time (= 826 (sparse-hash v256 benchmark-input)))
  )

(def suffix [17 31 73 47 23])

(defn ascii-code-input [s]
  (into (mapv int s) suffix))

(deftest ascii-code-input-test
  (testing "Ensure hex-ascii-code-input is producing the correct value"
    (is (= [49 44 50 44 51 17 31 73 47 23] (ascii-code-input "1,2,3")))))

;NOTE - One part of the problem description that is poorly specified is the
; vector that is supposed to be iterated on at each step. They have you use the
; same output at each step, but it is worded as if you use [0..255] each time.
(defn dense-hash-step [{:keys [res lengths skip pos]
                     :or      {skip 0 pos 0}
                     :as      m}]
  (let [[res pos skip] (knot-hash-round res lengths pos skip)]
    (assoc m :res res :pos pos :skip skip)))

(defn hex-hash [v]
  (->> (partition 16 v) (map #(format "%02x" (apply bit-xor %))) cs/join))

(deftest hex-hash-test
  (testing "Ensure hex-hash is producing the correct value"
    (is (= (Long/toHexString 64)
           (hex-hash [65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22])))))

(defn dense-hash [input]
  (->> {:res v256 :lengths (ascii-code-input input)}
       (iterate dense-hash-step)
       rest
       (map (comp hex-hash :res))
       (take 64)
       last))

(deftest part-2-example-hashes-test
  (testing "Ensure part 2 examples are correct"
    (is (= "a2582a3a0e66e6e86e3812dcb672a272" (dense-hash "")))
    (is (= "33efeb34ea91902bb2f59c9920caa6cd" (dense-hash "AoC 2017")))
    (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (dense-hash "1,2,3")))
    (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (dense-hash "1,2,4")))))

(comment
  (time (= "2da93395f1a6bb3472203252e3b17fe5" (dense-hash (cs/join "," input))))
  (time (= "d067d3f14d07e09c2e7308c3926605c4" (dense-hash (cs/join "," benchmark-input))))
  )
