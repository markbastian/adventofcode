(ns adventofcode.year2017.day17)

(defn step [{:keys [buffer current-pos value step-size] :as m}]
  (let [new-pos (inc (mod current-pos (count buffer)))
        [pre pos] (split-at new-pos buffer)]
    (-> m
        (assoc :buffer (into (vec pre) (cons value pos)))
        (assoc :current-pos (+ new-pos step-size))
        (update :value inc))))

(defn mstep [{:keys [current-pos value step-size] :as m}]
  (let [p (first (drop step-size (iterate m current-pos)))]
    (-> m
        (assoc :current-pos value)
        (assoc p value)
        (update :value inc)
        (assoc value (m p)))))

(comment
  (time
    (= 1025
       (->> {:buffer [0] :current-pos 0 :value 1 :step-size 366}
            (iterate step)
            (map :buffer)
            (drop 2017)
            first
            cycle
            (drop-while (complement #{2017}))
            rest
            first)))

  (time
    (= 1025
       (let [m (->> {0 0 :current-pos 0 :value 1 :step-size 366}
                    (iterate mstep)
                    (drop 2017)
                    first)]
         (m 2017))))

  ;The final answer. Let it crank all night?
  ; Took 1.2541228100911E7 msec ~ 3.5 hours.
  (time
    (= 37803463
       (let [m (->> {0 0 :current-pos 0 :value 1 :step-size 366}
                    (iterate mstep)
                    (drop 50000000)
                    first)]
         (m 0))))

  #_
  (time
    (->> {0 0 :current-pos 0 :value 1 :step-size 366}
         (iterate mstep)
         ;(drop 50000000)
         (map (fn [m] (m 0)))
         (take 1000000)
         (partition-by identity)
         (mapv (fn [[v :as s]] [v (count s)])))))