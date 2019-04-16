(ns adventofcode.year2018.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn split-cars [s]
  (reduce
    (fn [m [coord spc]]
      (case spc
        (\> \<) (-> m
                    (assoc-in [:cars coord] {:dir spc :turn :left})
                    (assoc-in [:track coord] \-))
        (\^ \v) (-> m
                    (assoc-in [:cars coord] {:dir spc :turn :left})
                    (assoc-in [:track coord] \|))
        (assoc-in m [:track coord] spc)))
    {:cars {} :track {}}
    s))

(defn parse-input [f]
  (let [lines (->> f io/resource slurp cs/split-lines vec)]
    (split-cars
      (for [row (range (count lines)) :let [line (into [] (lines row))]
            col (range (count line)) :let [v (line col)]
            :when ((complement #{\space}) v)]
        [[row col] v]))))

(def input1 (parse-input "adventofcode/year2018/day13/test_input1.txt"))
(def input2 (parse-input "adventofcode/year2018/day13/test_input2.txt"))
(def input3 (parse-input "adventofcode/year2018/day13/test_input3.txt"))
(def input (parse-input "adventofcode/year2018/day13/input.txt"))

(def moves {:left :straight :straight :right :right :left})

(defn new-dir [{:keys [dir turn] :as car} track]
  (if (= \+ track)
    (-> car
        (assoc :dir
               (case [dir turn]
                 [\> :left] \^
                 [\> :right] \v
                 [\< :left] \v
                 [\< :right] \^
                 [\^ :left] \<
                 [\^ :right] \>
                 [\v :left] \>
                 [\v :right] \<
                 dir))
        (update :turn moves))
    (assoc car :dir (case [dir track]
                      [\> \\] \v
                      [\> \/] \^
                      [\< \/] \v
                      [\< \\] \^
                      [\v \\] \>
                      [\v \/] \<
                      [\^ \/] \>
                      [\^ \\] \<
                      dir))))

(defn remove-crashes [{:keys [queue] :as m}]
  (cond-> m (empty? queue) (dissoc :crashes)))

;up and down are screen coords
(def dirs {\> [0 1] \< [0 -1] \^ [-1 0] \v [1 0] \X [0 0]})

(defn car-step [{:keys [cars track crashes] [f & r] :queue :as m}]
  ;Handle cars in the queue
  (if f
    (let [{:keys [dir] :as car} (cars f)
          new-pos (mapv + f (dirs dir))
          m (-> m (update :cars dissoc f) (assoc :queue r))]
      (cond
        ;A crash exists at this location, just add the car to the existing crashes
        (get crashes new-pos) (update-in m [:crashes new-pos] (conj car))
        ;There is a car in the new location - instantiate a crash
        (cars new-pos) (-> m
                           (update-in [:crashes new-pos] conj car)
                           (update :cars dissoc new-pos)
                           (update-in [:crashes new-pos] conj (cars new-pos))
                           (update :queue (partial remove #{new-pos})))
        ;The car safely moves forward
        :else (assoc-in m [:cars new-pos] (new-dir car (track new-pos)))))
    ;If there are no cars in the queue, re-enqueue all the cars
    (assoc m :queue (sort-by (juxt second first) (keys cars)))))

(defn find-first-crash-site [input]
  (->> (iterate car-step input)
       (map #(dissoc % :track))
       (remove (comp seq :queue))
       (filter (comp seq :crashes))
       first
       :crashes))

(defn find-last-car [input]
  (->> (iterate (comp remove-crashes car-step) input)
       (map #(dissoc % :track))
       (remove (comp seq :queue))
       (map :cars)
       (remove (fn [cars] (> (count cars) 1)))
       first))

(comment
  ;[3 0]
  (find-first-crash-site input1)
  ;[3 7]
  (find-first-crash-site input2)
  ;[0 2] is the first crash, but if you complete ticks, there are 3 at that
  ; generation at [0 2], [4 2], and [4 6]
  (find-first-crash-site input3)
  ;[54 50]
  (find-first-crash-site input))

(comment
  ;None - mutual destruction
  (find-last-car input1)
  ;None - mutual destruction
  (find-last-car input2)
  ;[4 6]
  (find-last-car input3)
  ;[100 50]
  (find-last-car input))