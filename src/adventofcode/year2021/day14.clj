(ns adventofcode.year2021.day14
  (:require [adventofcode.input-util :as iu]))

(defn parse [input]
  (let [[template _ & lines] (iu/read-as-strings input)]
    {:template         (vec template)
     :pair-frequencies (frequencies (partition 2 1 template))
     :rules            (->> lines
                            (map (fn [line]
                                   (let [[_ pre post] (re-matches #"([A-Z]+)\s*->\s*([A-Z]+)" line)]
                                     [(vec pre) (first post)])))
                            (into {}))}))

(def sample-input (parse "adventofcode/year2021/day14/sample-input.txt"))
(def input (parse "adventofcode/year2021/day14/input.txt"))

(defn accumulate [m [[f s] ct]]
  (-> m
      (update f (fnil + 0) ct)
      (update s (fnil + 0) ct)))

(defn step [{:keys [pair-frequencies rules] :as state}]
  (letfn [(expand [[[a b :as pr] ct]] (let [n (rules pr)] [[[a n] [n b]] ct]))]
    (assoc state
      :pair-frequencies
      (->> pair-frequencies (map expand) (reduce accumulate {})))))

(defn pairs->elements [{:keys [template]} freqs]
  (-> freqs
      (update (first template) inc)
      (update (peek template) inc)
      (update-vals #(quot % 2))))

(defn solution [input n]
  (let [freqs (->> input
                   (iterate step)
                   (map :pair-frequencies)
                   (drop n)
                   first
                   (reduce accumulate {})
                   (pairs->elements input))
        [_ hi] (apply max-key second freqs)
        [_ lo] (apply min-key second freqs)]
    (- hi lo)))

(comment
  (= 1588 (solution sample-input 10))
  (= 2988 (solution input 10))
  (= 2188189693529 (solution sample-input 40))
  (= 3572761917024 (solution input 40))
  )