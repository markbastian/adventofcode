(ns adventofcode.year2019.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def input [1 9 10 3 2 3 11 0 99 30 40 50])

(defn digits [n]
  (loop [r (quot n 10) res (list (rem n 10))]
    (if (zero? r)
      res
      (recur (quot r 10) (conj res (rem r 10))))))

(defn step1 [{:keys [address memory input] :as state}]
  (let [[opcode a b c] (subvec memory address)
        code (rem opcode 100)
        [x y] (into (repeat 0) (digits (quot opcode 100)))]
    (case code
      1 (-> state
            (update :address + 4)
            (assoc-in [:memory c]
                      (+
                        (if (zero? x) (memory a) a)
                        (if (zero? y) (memory b) b))))
      2 (-> state
            (update :address + 4)
            (assoc-in [:memory c]
                      (*
                        (if (zero? x) (memory a) a)
                        (if (zero? y) (memory b) b))))
      3 (-> state
            (update :address + 2)
            (assoc-in [:memory a] input))
      4 (-> state
            (update :address + 2)
            (assoc :output (memory a)))
      99 (assoc state :halted? true))))

(defn step2 [{:keys [address memory input] :as state}]
  (let [[opcode & r] (subvec memory address)
        code (rem opcode 100)
        modes (into (repeat 0) (digits (quot opcode 100)))
        params (map
                 (fn [param mode]
                   (if (zero? mode)
                     (get memory param)
                     param))
                 r modes)]
    (case code
      1 (let [[a b c] params]
          (-> state
              (update :address + 4)
              (assoc-in [:memory c] (+ a b))))
      2 (let [[a b c] params]
          (-> state
              (update :address + 4)
              (assoc-in [:memory c] (* a b))))
      3 (let [[a] params]
          (-> state
              (update :address + 2)
              (assoc-in [:memory a] input)))
      4 (let [[a] params]
          (-> state
              (update :address + 2)
              (assoc :output a)))
      5 (let [[a b] params]
          (cond-> state (not (zero? a)) (assoc :address b)))
      6 (let [[a b] params]
          (cond-> state (zero? a) (assoc :address b)))
      7 (let [[p1 p2 p3] params
              v (if (< p1 p2) 1 0)]
          (-> state
              (update :address + 4)
              (assoc-in [:memory p3] v)))
      8 (let [[p1 p2 p3] params
              v (if (= p1 p2) 1 0)]
          (-> state
              (update :address + 4)
              (assoc-in [:memory p3] v)))
      99 (assoc state :halted? true))))

(def input
  (mapv
    #(Long/parseLong %)
    (-> "adventofcode/year2019/day05/input.txt" io/resource slurp (cs/split #","))))

(comment

  (->> {:address 0 :memory [3 0 4 0 99] :input "Test"}
       (iterate step1)
       (take-while (fn [{:keys [halted?]}] (not halted?))))

  (->> {:address 0 :memory [1002 4 3 4 33] :input "Test"}
       (iterate step1)
       (take-while (fn [{:keys [halted?]}] (not halted?))))

  (= 9219874
     (->> {:address 0 :memory input :input 1}
          (iterate step1)
          (take-while (fn [{:keys [halted?]}] (not halted?)))
          last
          :output))

  ;Stage 2
  (= 1
     (->> {:address 0 :memory [3 9 8 9 10 9 4 9 99 -1 8] :input 8}
          (iterate step2)
          (take-while (fn [{:keys [halted?]}] (not halted?)))
          last
          :output))

  (= 0
     (->> {:address 0 :memory [3 9 7 9 10 9 4 9 99 -1 8] :input 8}
          (iterate step2)
          (take-while (fn [{:keys [halted?]}] (not halted?)))
          last
          :output))

  (= 1
     (->> {:address 0 :memory [3 3 1108 -1 8 3 4 3 99] :input 8}
          (iterate step2)
          (take-while (fn [{:keys [halted?]}] (not halted?)))
          last
          :output))

  (= 0
     (->> {:address 0 :memory [3 3 1107 -1 8 3 4 3 99] :input 8}
          (iterate step2)
          (take-while (fn [{:keys [halted?]}] (not halted?)))
          last
          :output))

  (= 0
     (->> {:address 0 :memory [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] :input 1}
          (iterate step2)
          (take-while (fn [{:keys [halted?]}] (not halted?)))
          last
          :output))

  (= 0
     (->> {:address 0 :memory [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] :input 1}
          (iterate step2)
          (take-while (fn [{:keys [halted?]}] (not halted?)))
          last
          :output))

  (= 9219874
     (->> {:address 0 :memory input :input 5}
          (iterate step2)
          (take-while (fn [{:keys [halted?]}] (not halted?)))
          last
          :output))

  )