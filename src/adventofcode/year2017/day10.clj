(ns adventofcode.year2017.day10
  (:require [clojure.string :as cs]))

(defn swap-pos [v a b]
  (let [a (mod a (count v))
        b (mod b (count v))]
    (assoc v a (v b) b (v a))))

(defn rev-len [v from len]
  (letfn [(rev-block [v from to]
            (if (>= from to)
              v
              (rev-block (swap-pos v from to) (inc from) (dec to))))]
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
  (knot-hash-run [0 1 2 3 4] [3 4 1 5])
  (knot-hash-round [0 1 2 3 4] [3 4 1 5]))

(defn step-1-hash [v lens]
  (->> (knot-hash-round v lens) first (take 2) (apply *)))

(def lengths [227 169 3 166 246 201 0 47 1 255 2 254 96 3 97 144])
(def v256 (vec (range 256)))

(comment
  (step-1-hash [0 1 2 3 4] [3 4 1 5])
  (= 13760 (step-1-hash v256 lengths)))

(def suffix [17 31 73 47 23])

(defn ascii-code-input [s]
  (into (mapv int s) suffix))

;NOTE - One part of the problem description that is poorly specified is the
; vector that is supposed to be iterated on at each step. They have you use the
; same output at each step, but it is worded as if you use [0..255] each time.
(defn stage-2-step [{:keys [res lengths skip pos]
                     :or   {skip 0 pos 0}
                     :as   m}]
  (let [[res pos skip] (knot-hash-round res lengths pos skip)]
    (assoc m :res res :pos pos :skip skip)))

(defn hex-hash [v]
  (->> (partition 16 v) (map #(format "%02x" (apply bit-xor %))) cs/join))

(defn stage-2-hash [input]
  (->> {:res v256 :lengths (ascii-code-input input)}
       (iterate stage-2-step)
       rest
       (map (comp hex-hash :res))
       (take 64)
       last))

(comment
  (= [49 44 50 44 51 17 31 73 47 23] (ascii-code-input "1,2,3"))
  (= "40" (hex-hash [65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22]))

  (= "a2582a3a0e66e6e86e3812dcb672a272" (stage-2-hash ""))
  (= "33efeb34ea91902bb2f59c9920caa6cd" (stage-2-hash "AoC 2017"))
  (= "3efbe78a8d82f29979031a4aa0b16a9d" (stage-2-hash "1,2,3"))
  (= "63960835bcdc130f0b66d7ff4f6a5a8e" (stage-2-hash "1,2,4"))
  (= "2da93395f1a6bb3472203252e3b17fe5" (stage-2-hash (cs/join "," lengths))))
