(ns adventofcode.year2021.day02
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [adventofcode.input-util :as iu]))

(def sample-input '[forward 5 down 5 forward 8 up 3 down 8 forward 2])

(defonce input (iu/read-as-array "adventofcode/year2021/day02/input.txt"))

(defn command-seq-1 [pos [op x]]
  (case op
    forward (update pos :horizontal-position (fnil + 0) x)
    up (update pos :depth (fnil - 0) x)
    down (update pos :depth (fnil + 0) x)))

(defn command-seq-2 [{:keys [aim] :as pos :or {aim 0}} [op x]]
  (case op
    forward (-> pos
                (update :horizontal-position (fnil + 0) x)
                (update :depth (fnil + 0) (* aim x)))
    up (update pos :aim (fnil - 0) x)
    down (update pos :aim (fnil + 0) x)))

(defn command-seq [cs input]
  (reduce cs {} (partition 2 input)))

(defn step [s]
  (let [{:keys [horizontal-position depth]} s]
    (* horizontal-position depth)))

(comment
  (= 150 (step (command-seq command-seq-1 sample-input)))
  (= 1938402 (step (command-seq command-seq-1 input)))
  (= 900 (step (command-seq command-seq-2 sample-input)))
  (= 1947878632 (step (command-seq command-seq-2 input))))
