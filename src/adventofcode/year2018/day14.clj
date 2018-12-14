(ns adventofcode.year2018.day14
  (:require [clojure.string :as cs]))

(def scores [3 7])

(defn digits [n]
  (loop [d (quot n 10) x (list (rem n 10))]
    (if (zero? d)
      x
      (recur (quot d 10) (conj x (rem d 10))))))

(defn s [[elf0 elf1 recipes]]
  (let [r (into recipes (digits (+ (recipes elf0) (recipes elf1))))]
    [(rem (+ elf0 (recipes elf0) 1) (count r))
     (rem (+ elf1 (recipes elf1) 1) (count r))
     r]))

;This is the solution that worked for part 2
(defn lr [[elf0 elf1 recipes] goal]
  (let [[f1 & g1 :as goal-digs] (digits goal)]
    (letfn [(new-recipes [e0 e1 r] (digits (+ (r e0) (r e1))))
            (next-pos [p r c] (rem (+ p (r p) 1) c))]
      (loop [r recipes
             e0 elf0
             e1 elf1
             [f & d] (new-recipes e0 e1 r)
             n 0
             [c & x :as g] goal-digs]
        (cond
          (nil? c) (- (count r) (count goal-digs))
          (= f c) (recur (conj r f) e0 e1 d (inc n) x)
          (= f f1) (recur (conj r f) e0 e1 d (inc n) g1)
          f (recur (conj r f) e0 e1 d (inc n) goal-digs)
          :else (let [nr (new-recipes e0 e1 r) l (+ (count r) (count nr))]
                      (recur r (next-pos e0 r l) (next-pos e1 r l) nr (inc n) g)))))))

(def score-seq
  (->> [0 1 scores]
       (iterate s)
       (map peek)))

(defn part1 [n]
  (->> score-seq
       (map (fn [v] (when (>= (count v) (+ n 10)) (subvec v n (+ n 10)))))
       (filter identity)
       first
       cs/join))

(defn peek-filter [n s]
  (->> (filter (fn [v] (= n (peek v))) s)
       (map pop)))

(defn digits-filter [digits s]
  (reduce #(peek-filter %2 %1) s (reverse digits)))

;does this miss if 10 gets added?
(defn part2 [input]
  (let [d (vec (digits input))]
    (->> score-seq (digits-filter d) first count)))

;way slow
(defn part2a [input]
  (let [d (apply str (digits input))
        soln (->> score-seq (map cs/join) (filter #(cs/includes? % d)) first)
        [ans] (cs/split soln (re-pattern d))]
    (count ans)))

;(part2 51589)
;(part2 59414)
;(part2 880751)

(defn part2x [input]
  (let [d (vec (digits input))
        n (count d)
        soln (->> score-seq
                  (remove (fn [v] (< (count v) n)))
                  (filter (fn [v] (= d (subvec v (- (count v) n)))))
                  first)]
    (- (count soln) n)))