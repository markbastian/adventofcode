(ns adventofcode.year2021.day10
  (:require [adventofcode.input-util :as iu]))

(def sample-input (iu/read-as-strings "adventofcode/year2021/day10/sample-input.txt"))
(def input (iu/read-as-strings "adventofcode/year2021/day10/input.txt"))

(def o->c {\< \> \{ \} \[ \] \( \)})
(def c->o (zipmap (vals o->c) (keys o->c)))

(defn process [line]
  (loop [[nxt & r] line stack []]
    (cond
      (o->c nxt) (recur r (conj stack nxt))
      (c->o nxt)
      (if-some [opener (peek stack)]
        (if (= opener (c->o nxt))
          (recur r (pop stack))
          {:found nxt :expected (o->c opener)})
        {:found nxt})
      :default
      (if (seq stack)
        {:stack      stack
         :completion (->> stack reverse (map o->c))}
        :balanced!))))

(defn part1 [input]
  (let [costs {\) 3 \] 57 \} 1197 \> 25137}]
    (->> input
         (map process)
         (filter :found)
         (map (comp costs :found))
         (apply +))))

(defn score [completion]
  (let [scores {\) 1 \] 2 \} 3 \> 4}]
    (reduce
      (fn [score c] (+ (* 5 score) (scores c)))
      0
      completion)))

(defn part2 [input]
  (let [scores (->> input
                    (map process)
                    (filter :completion)
                    (map (comp score :completion))
                    sort)]
    (first (drop (quot (count scores) 2) scores))))

(comment
  (= 26397 (part1 sample-input))
  (= 462693 (part1 input))
  (= 288957 (part2 sample-input))
  (= 3094671161 (part2 input))
  )