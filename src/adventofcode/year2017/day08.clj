(ns adventofcode.year2017.day08
  (:require [clojure.edn :as edn]
            [clojure.string :as cs]
            [clojure.java.io :as io]))

(def input
  "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(def ops {'> > '>= >= '< < '<= <= '== = '!= not= 'inc + 'dec -})

(defn execute-instr [state instr]
  (let [[register inc-or-dec amt _ pre cnd post] (edn/read-string (str "[" instr "]"))]
    (letfn [(updater [v]
              (cond-> v
                      ((ops cnd) (state (keyword pre) 0) post)
                      ((ops inc-or-dec) amt)))]
      (update state (keyword register) (fnil updater 0)))))

(defn execute-instrs [instrs]
  (reduce execute-instr {} (cs/split-lines instrs)))

(comment
  (= {:b 0, :a 1, :c -10} (execute-instrs input))

  (= 6828
     (->> "adventofcode/year2017/day08/input.txt"
          io/resource
          slurp
          execute-instrs
          vals
          (apply max))))

;part b
(comment
  (= 7234
     (->> "adventofcode/year2017/day08/input.txt"
          io/resource
          slurp
          cs/split-lines
          (reductions execute-instr {})
          (mapcat vals)
          (apply max))))