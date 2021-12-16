(ns adventofcode.year2021.day15
  (:require [adventofcode.input-util :as iu]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.pprint :as pp]))

(defn parse [s]
  (->> (iu/read-as-strings s)
       (mapv (fn [s] (mapv (fn [c] (- (int c) (int \0))) s)))))

(def sample-input (parse "adventofcode/year2021/day15/sample-input.txt"))
(def input (parse "adventofcode/year2021/day15/input.txt"))

;; Note - Code lifted from my own planning library at
;; https://github.com/markbastian/planning/blob/master/src/main/cljc/planning/core.cljc

(defn grid-neigbors [neighbors grid c]
  (filter (partial get-in grid) (neighbors c)))

(defn von-neumann-neighbors
  "Return the 8 neighboring cells in a cartesian grid.
  https://en.wikipedia.org/wiki/Von_Neumann_neighborhood"
  ([[x y]]
   (let [i ((juxt inc identity dec identity) x)
         j ((juxt identity inc identity dec) y)]
     (map vector i j)))
  ([grid c]
   (filter
     (fn [neighbor] (get-in grid neighbor))
     (grid-neigbors von-neumann-neighbors grid c))))

(defn- update-frontier [m additions]
  (update m :frontier #(-> % pop (into additions))))

(defn- update-visited [m from-state new-neighbors]
  (update m :visited into (zipmap new-neighbors (repeat from-state))))

(defn- neighbors-with-costs [{:keys [frontier neighbors-fn cost-fn costs visited]}]
  (let [[current-state] (peek frontier)]
    (for [neighbor (neighbors-fn current-state)
          :let [new-cost (+ (costs current-state) (cost-fn current-state neighbor))
                old-cost (costs neighbor ##Inf)]
          :when (< new-cost old-cost)]
      [neighbor new-cost])))

(defn- initialize [{:keys [frontier cost-fn start] :as m}]
  (cond->
    (assoc m
      :visited {start nil}
      :frontier (if frontier (conj frontier start) (priority-map start 0)))
    cost-fn (assoc :costs {start 0})))

(defn- search-seq [algorithm-step-fn]
  (fn [initial-conditions]
    (->> (initialize initial-conditions)
         (iterate algorithm-step-fn)
         (take-while (comp seq :frontier)))))

(defn A-star-step [{:keys [heuristic-fn goal frontier] :as m}]
  (let [[current-state] (peek frontier)
        costs (neighbors-with-costs m)
        estimates (map (fn [[s c]] [s (+ c (heuristic-fn goal s))]) costs)]
    (-> m
        (update-frontier estimates)
        (update :costs into costs)
        (update-visited current-state (map first costs)))))

(def A-star-seq (search-seq A-star-step))

(defn find-goal-state [search-seq]
  (first (filter (fn [{:keys [goal visited]}] (visited goal)) search-seq)))

(def A-star-terminus (comp find-goal-state A-star-seq))

(defn recover-path [{:keys [goal visited]}]
  (when-some [visited-goal (-> visited keys set (get goal))]
    (vec (reverse (take-while some? (iterate visited visited-goal))))))

(def A-star-search (comp recover-path A-star-terminus))

(defn manhattan-distance [a b]
  (reduce + (map (comp #(Math/abs ^long %) -) a b)))

(defn solve-path-A* [input]
  (let [w (dec (count input))
        h (dec (count (input 0)))
        config {:start        [0 0]
                :goal         [w h]
                :neighbors-fn (partial von-neumann-neighbors input)
                :heuristic-fn manhattan-distance
                :cost-fn      (fn [_from to] (get-in input to))}
        solution (A-star-search config)]
    (reduce + (map #(get-in input %) (rest solution)))))

(defn expand-input [input]
  (letfn [(expand-row [row]
            (->> (for [i (range 5)] (map #(+ i %) row)) flatten vec))
          (wrap [col] (if (> col 9) (inc (rem col 10)) col))]
    (let [m (->> (mapv expand-row input)
                 (apply mapv vector)
                 (mapv expand-row)
                 (apply mapv vector))]
      (mapv (fn [row] (mapv wrap row)) m))))

(comment
  (time (count (expand-input input)))
  ;; Unfortunately dialing up the power of the heuristic leads to wrong answers
  ;; that are very close to the right one. I need a better heuristic.
  (= 40 (solve-path-A* sample-input))
  (= 537 (solve-path-A* input))
  (= 315 (solve-path-A* (expand-input sample-input)))
  ; 10X faster, but not min-cost
  (= 2881 (time (solve-path-A* (expand-input input))))
  )