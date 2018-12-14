(ns adventofcode.year2018.day09)

(def m {:marble 0 :curr 0 0 {:next 0 :prev 0}})

(defn setup [m num-players]
  (into m {:player 1 :players (zipmap (map inc (range num-players)) (repeat 0))}))

(defn n [{:keys [curr] :as m}]
  (assoc m :curr (-> curr m :next)))

(defn p [{:keys [curr] :as m}]
  (assoc m :curr (-> curr m :prev)))

(defn insert [{:keys [curr] :as m} v]
  (let [{:keys [next]} (m curr)]
    (-> m
        (assoc-in [curr :next] v)
        (assoc v {:next next :prev curr})
        (assoc-in [next :prev] v)
        n)))

(defn remove-curr [{:keys [curr] :as m}]
  (let [{:keys [next prev]} (m curr)]
    (-> m
        (assoc-in [prev :next] next)
        (assoc-in [next :prev] prev)
        (dissoc curr)
        (assoc :curr prev)
        n)))

(defn step [{:keys [player players marble] :as m}]
  (let [nxt (inc marble)]
    (assoc
      (if (zero? (mod nxt 23))
        (let [{v :curr :as rew} (nth (iterate p m) 7)]
          (-> rew remove-curr (update-in [:players player] + nxt v)))
        (insert (n m) nxt))
      :player (if (= player (count players)) 1 (inc player))
      :marble nxt)))

;430 players; last marble is worth 71588 points
;(scores 430 71588)
;=> [127 422748]
;(scores 430 7158800)
;=> [402 3412522480]
(defn scores [players hi-marble]
  (->> (setup m players)
       (iterate step)
       (map :players)
       (drop hi-marble)
       first
       (apply max-key second)))

