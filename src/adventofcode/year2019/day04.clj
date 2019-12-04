(ns adventofcode.year2019.day04)

(defn digits [n]
  (loop [r (quot n 10) res (list (rem n 10))]
    (if (zero? r)
      res
      (recur (quot r 10) (conj res (rem r 10))))))

(defn nondecreasing [d] (apply <= d))

(defn adjacent-digits? [d]
  (true? (some (fn [s] (> (count s) 1)) (partition-by identity d))))

(defn adjacent-pair? [d]
  (true? (some (fn [s] (= (count s) 2)) (partition-by identity d))))

(defn stage-1-pass? [d]
  (let [digs (digits d)]
    (and
      (= 6 (count digs))
      (nondecreasing digs)
      (adjacent-digits? digs))))

(defn stage1 [lo hi]
  (count (filter stage-1-pass? (range lo hi))))

(defn stage-2-pass? [d]
  (let [digs (digits d)]
    (and
      (= 6 (count digs))
      (nondecreasing digs)
      (adjacent-pair? digs))))

(defn stage2 [lo hi]
  (count (filter stage-2-pass? (range lo hi))))

(comment
  (= true (stage-1-pass? 111111))
  (= false (stage-1-pass? 223450))
  (= false (stage-1-pass? 123789))

  (= 1178 (stage1 235741 706948))

  (= true (stage-2-pass? 112233))
  (= false (stage-2-pass? 123444))
  (= true (stage-2-pass? 111122))

  (= 763 (stage2 235741 706948))
  )
