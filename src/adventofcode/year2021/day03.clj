(ns adventofcode.year2021.day03
  (:require [adventofcode.input-util :as iu]
            [clojure.string :as str]))

(def sample-input
  ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])

(def input (iu/read-as-strings "adventofcode/year2021/day03/input.txt"))

(defn bitsort [v]
  (let [{n0 \0 n1 \1} (frequencies v)]
    (if (> n0 n1) [\0 \1] [\1 \0])))

(defn power-consumption [input]
  (->> input
       (apply map vector)
       (map bitsort)
       (apply map vector)
       (map (comp #(Long/parseLong % 2) str/join))
       (zipmap [:gamma-rate :epsilon-rate])))

(defn score [f input]
  (->> input f vals (apply *)))

(defn step [{[mf ms :as m] :m :keys [goal col] :as x}]
  (if ms
    (let [[mfb lfb] (bitsort (map #(get % col) m))
          b (if (= goal :mfb) mfb lfb)]
      (-> (update x :col inc)
          (update :m (partial filter (fn [row] (= b (row col)))))))
    (assoc x :rating (Long/parseLong (str/join mf) 2))))

(defn life-support-rating [input]
  (let [seed {:col 0 :m (mapv vec input) :goal :mfb}]
    (letfn [(process [v] (->> v (iterate step) (map :rating) (filter identity) first))]
      {:oxygen-generator-rating (process seed)
       :co2-scrubber-rating     (process (assoc seed :goal :lfb))})))

(comment
  (power-consumption sample-input)
  (= 198 (score power-consumption sample-input))
  (power-consumption input)
  (= 2648450 (score power-consumption input))

  (life-support-rating sample-input)
  (= 230 (score life-support-rating sample-input))
  (life-support-rating input)
  (= 2845944 (score life-support-rating input))
  )
