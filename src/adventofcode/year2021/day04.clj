(ns adventofcode.year2021.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-board [m]
  (letfn [(parse-row [r] (mapv #(Long/parseLong %) (str/split (str/trim r) #"\s+")))]
    (mapv parse-row m)))

(defn reverse-index [board]
  (reduce
    (fn [m c] (assoc m (get-in board c) c))
    {}
    (for [i (range 5) j (range 5)] [i j])))

(defn parse-input [res]
  (let [[f & r] (-> res
                    io/resource
                    slurp
                    str/split-lines)
        calls (map #(Long/parseLong %) (str/split f #","))
        boards (->> (partition 6 r)
                    (map rest)
                    (mapv parse-board))]
    {:calls  calls
     :boards (mapv (fn [board reverse-index]
                     {:board         board
                      :marked-board  board
                      :reverse-index reverse-index})
                   boards
                   (mapv reverse-index boards))}))

(def sample-input (parse-input "adventofcode/year2021/day04/sample-input.txt"))
(def input (parse-input "adventofcode/year2021/day04/input.txt"))

(defn row-marked [marked-board]
  (some (partial every? #{:X}) marked-board))

(defn col-marked [marked-board]
  (row-marked (apply map vector marked-board)))

(defn maybe-win [{:keys [marked-board] :as board}]
  (cond-> board
          (or (row-marked marked-board) (col-marked marked-board))
          (assoc :win true)))

(defn mark-board [{:keys [reverse-index] :as b} value]
  (if-some [[i j] (reverse-index value)]
    (-> b
        (assoc-in [:marked-board i j] :X)
        (update :marker-seq (comp vec conj) value)
        maybe-win)
    b))

(defn step [{[n & r] :calls
             :keys   [boards winners]
             :or     {winners []}
             :as     b}]
  (let [{w true l false} (->> boards
                              (map (fn [board] (mark-board board n)))
                              (group-by (comp true? :win)))]
    (assoc b
      :winners (into winners w)
      :boards l
      :calls r)))

(defn score [{:keys [marker-seq marked-board]}]
  (* (peek marker-seq) (apply + (remove #{:X} (flatten marked-board)))))

(defn part-1-score [input]
  (->> input
       (iterate step)
       (filter (comp seq :winners))
       first
       :winners
       first
       score))

(defn part-2-score [input]
  (->> input
       (iterate step)
       (remove (comp seq :boards))
       first
       :winners
       last
       score))

(comment
  ;; Part 1
  (= 4512 (part-1-score sample-input))
  (= 11536 (part-1-score input))
  ;; Part 2
  (= 1924 (part-2-score sample-input))
  (= 1284 (part-2-score input))
  )