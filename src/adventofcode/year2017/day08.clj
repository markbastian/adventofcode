(ns adventofcode.year2017.day08
  (:require [clojure.edn :as edn]
            [clojure.string :as cs]
            [clojure.java.io :as io]))

(def input
  "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(def rgx #"(.)\s+(inc|dec)\s+\d+\s+if\s+(.)(>|>=|==|<|<=|!=)\s+\d+")

(re-matches rgx "b inc 5 if a > 1")

(def ops {'> > '>= >= '< < '<= <= '== = '!= not= 'inc + 'dec -})

(defn execute-instr [state instr]
  (let [[reg inc-or-dec amt _ pre cnd post]
        (edn/read-string (str "[" instr "]"))]
    (update state (keyword reg)
            (fn [v]
              (let [v (or v 0)]
                (if ((ops cnd) (state (keyword pre) 0) post)
                  ((ops inc-or-dec) v amt)
                  v))))))

(defn execute-instrs [instrs]
  (reduce execute-instr {} (cs/split-lines instrs)))

(execute-instrs input)

(comment
  (->> "adventofcode/year2017/day08/input.txt"
       io/resource
       slurp
       execute-instrs
       vals
       (apply max)))

;part b
(comment
  (->> "adventofcode/year2017/day08/input.txt"
       io/resource
       slurp
       cs/split-lines
       (reductions execute-instr {})
       (mapcat vals)
       (apply max)))