(ns adventofcode.year2020.day08
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(defn read-program [input]
  (->> (io/resource input)
       slurp
       cs/split-lines
       (mapv (fn [s]
               (let [[op jump] (cs/split s #"\s+")]
                 {:op (keyword op) :jump (Long/parseLong jump)})))))

(defn step [{:keys [program index] :as state}]
  (let [{:keys [op jump] :as instruction} (program (mod index (count program)))]
    (-> (case op
          :nop (update state :index inc)
          :acc (-> state (update :value + jump) (update :index inc))
          :jmp (update state :index + jump))
        (assoc :instruction instruction))))

(defn step1 [input]
  (let [program (read-program input)]
    (->> (iterate step {:program program :index 0 :value 0})
         rest
         (map #(dissoc % :program))
         (reduce
           (fn [{:keys [indices values] :as acc} {:keys [index value]}]
             (if (get indices index)
               (reduced (first values))
               (-> acc
                   (update :indices (comp set conj) index)
                   (update :values conj value))))
           {}))))

(comment
  (time (= 5 (step1 "adventofcode/year2020/day08/test-input.txt")))
  (time (= 5 (step1 "adventofcode/year2020/day08/input.txt"))))