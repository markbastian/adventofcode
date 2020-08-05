(ns adventofcode.year2017.day09
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def input
  "{}
{{{}}}
{{},{}}
{{{},{},{{}}}}
{<a>,<a>,<a>,<a>}
{{<ab>},{<ab>},{<ab>},{<ab>}}
{{<!!>},{<!!>},{<!!>},{<!!>}}
{{<a!>},{<a!>},{<a!>},{<ab>}}")

(defn step [{:keys [input stack] :as m}]
  (when-some [f (first input)]
    (let [t (peek stack)
          m (update m :input rest)]
      (if (= t \<)
        (case f
          \! (update m :input rest)
          \> (update m :stack pop)
          (update m :garbage inc))
        (case f
          \! (update m :input rest)
          (\{ \<) (update m :stack conj f)
          \} (-> m (update :stack pop) (update :score + (count stack)))
          m)))))

(defn process [input]
  (->> {:input input :stack [] :score 0 :garbage 0}
       (iterate step)
       (drop-while (comp seq :input))
       first))

(comment
  (step (step {:input "{{{},{},{{}}}}" :stack [] :score 0 :garbage 0}))

  (->> {:input "{{<a!>},{<a!>},{<a!>},{<ab>}}" :stack [] :score 0 :garbage 0}
       (iterate step)
       (map #(dissoc % :input))
       (take-while identity))

  (= [1 6 5 16 1 9 9 3]
     (->> input
          cs/split-lines
          (map process)
          (mapv :score)))

  ;12897
  (->> "adventofcode/year2017/day09/input.txt" io/resource slurp process time))