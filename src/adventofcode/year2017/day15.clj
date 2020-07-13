(ns adventofcode.year2017.day15)

(def mask (partial bit-and 0xFFFF))

(defn generator [seed factor]
  (iterate (fn [v] (rem (* factor v) 2147483647)) seed))

(defn match-seq [ga gb]
  (map = (map mask ga) (map mask gb)))

(comment
  (= 588
     (->> (match-seq
            (generator 65 16807)
            (generator 8921 48271))
          (take 40000000)
          (filter true?)
          count))

  (= 573
     (->> (match-seq
            (generator 634 16807)
            (generator 301 48271))
          (take 40000000)
          (filter true?)
          count)))

(defn filter-multiple [multiple s]
  (filter #(zero? (rem % multiple)) s))

(comment
  (= 309
     (->> (match-seq
            (filter-multiple 4 (generator 65 16807))
            (filter-multiple 8 (generator 8921 48271)))
          (take 5000000)
          (filter true?)
          count))

  (= 294
     (->> (match-seq
            (filter-multiple 4 (generator 634 16807))
            (filter-multiple 8 (generator 301 48271)))
          (take 5000000)
          (filter true?)
          count)))