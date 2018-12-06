(ns adventofcode.year2018.day05
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def input "dabAcCaCBAcCcaDA")

(def cancellable-pairs
  (set (for [i (range 26) :let [l (char (+ i (int \a))) L (char (+ i (int \A)))]] #{l L})))

(defn simplify-once [input]
  (loop [[f & r] input res []]
    (if (seq r)
      (if (cancellable-pairs (conj #{f} (first r)))
        (recur (rest r) res)
        (recur r (conj res f)))
      (cs/join (cond-> res f (conj f))))))

(defn brute-force-simplify [s]
  (->> (iterate simplify-once s)
       (partition 2 1)
       (drop-while (fn [[a b]] (not= a b)))
       ffirst))

(defn simplify-indexes
  ([s i]
   (if-some [a (get s i)]
     (let [b (get s (inc i))
           pr (cond-> #{a} b (conj b))]
       (if (cancellable-pairs pr)
         (let [pre (subs s 0 i)
               post (subs s (min (count s) (+ i 2)))]
           (recur (str pre post) (max 0 (dec i))))
         (recur s (inc i))))
     s))
  ([s] (simplify-indexes s 0)))

(defn simplify-stack [input]
  (loop [[f & r] input stack []]
    (if f
      (let [t (peek stack) pr (cond-> #{f} t (conj t))]
        (recur r (if (cancellable-pairs pr) (pop stack) (conj stack f))))
      (cs/join stack))))

(comment
  (= "dabCBAcaDA"
     (brute-force-simplify input)
     (simplify-indexes input)
     (simplify-stack input))
  ;9154
  (->> "adventofcode/year2018/day05/input.txt" io/resource slurp brute-force-simplify count)
  ;This is way faster
  (time (->> "adventofcode/year2018/day05/input.txt" io/resource slurp simplify-indexes count))
  ;This is way, way faster
  (time (->> "adventofcode/year2018/day05/input.txt" io/resource slurp simplify-stack count)))


(defn brute-force-pair-simplifier [input]
  (zipmap
    cancellable-pairs
    (pmap (fn [pr] (count (brute-force-simplify (remove pr input)))) cancellable-pairs)))

(defn index-pair-simplifier [input]
  (zipmap
    cancellable-pairs
    (pmap (fn [pr] (count (simplify-indexes (cs/join (remove pr input))))) cancellable-pairs)))

(defn stack-pair-simplifier [input]
  (zipmap
    cancellable-pairs
    (pmap (fn [pr] (count (simplify-stack (cs/join (remove pr input))))) cancellable-pairs)))

(comment
  (brute-force-pair-simplifier input)
  (->> "adventofcode/year2018/day05/input.txt" io/resource slurp brute-force-pair-simplifier)
  ;Much faster
  (->> "adventofcode/year2018/day05/input.txt" io/resource slurp index-pair-simplifier)
  ;By far the fastest
  (->> "adventofcode/year2018/day05/input.txt" io/resource slurp stack-pair-simplifier))

{#{\D \d} 8780
 #{\B \b} 8806
 #{\N \n} 8776
 #{\L \l} 8770
 #{\G \g} 8774
 #{\W \w} 8780
 #{\R \r} 8802
 #{\S \s} 8808
 #{\J \j} 8808
 #{\H \h} 8798
 #{\O \o} 8798
 #{\E \e} 4556
 #{\Y \y} 8854
 #{\A \a} 8844
 #{\Z \z} 8756
 #{\F \f} 8768
 #{\C \c} 8772
 #{\P \p} 8808
 #{\M \m} 8768
 #{\I \i} 8780
 #{\K \k} 8774
 #{\V \v} 8746
 #{\U \u} 8798
 #{\X \x} 8742
 #{\Q \q} 8766
 #{\T \t} 8780}
