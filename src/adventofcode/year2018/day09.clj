(ns adventofcode.year2018.day09)

(defn step [s n]
  (let [[f & r] (cycle s)]
    (take (inc (count s)) (drop 1 (cons f (cons n r))))))

(defn s [{:keys [v p n]}]
  (let [i (if (seq v) (rem (+ 1 p) (count v)) 0)
        [a b] (split-at i v)]
    {:v (into (conj (vec a) n) b)
     :p i
     :n (inc n)}))
