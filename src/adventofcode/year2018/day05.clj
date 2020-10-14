(ns adventofcode.year2018.day05
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def test-input "dabAcCaCBAcCcaDA")
(def input (->> "adventofcode/year2018/day05/input.txt" io/resource slurp))

(def cancellable-pairs
  (set (for [i (range 26)
             :let [l (char (+ i (int \a)))
                   L (char (+ i (int \A)))]]
         #{l L})))

(defn simplify-stack [input]
  (loop [[f & r] input stack []]
    (if f
      (let [t (peek stack)
            pr (cond-> #{f} t (conj t))]
        (recur r (if (cancellable-pairs pr)
                   (pop stack)
                   (conj stack f))))
      (cs/join stack))))

(comment
  (time (= "dabCBAcaDA" (simplify-stack test-input)))
  (time (= 9154 (->> input simplify-stack count))))

(defn stack-pair-simplifier [input]
  (letfn [(remove-and-simplify [pr]
            (->> input (remove pr) simplify-stack count))]
    (zipmap
      cancellable-pairs
      (pmap remove-and-simplify cancellable-pairs))))

(comment
  (time
    (= 4556
       (let [m (stack-pair-simplifier input)]
         (apply min (vals m))))))