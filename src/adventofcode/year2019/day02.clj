(ns adventofcode.year2019.day02)

(def input [1 9 10 3 2 3 11 0 99 30 40 50])

(defn step [[p v]]
  (let [[op a b c] (subvec v p)]
    (case op
      1 [(+ p 4) (assoc v c (+ (v a) (v b)))]
      2 [(+ p 4) (assoc v c (* (v a) (v b)))]
      99 [nil v])))

(def input1 [1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 6 1 19 1 5 19 23 1 13 23 27 1 6 27 31 2 31 13 35 1 9 35 39 2 39 13 43 1
             43 10 47 1 47 13 51 2 13 51 55 1 55 9 59 1 59 5 63 1 6 63 67 1 13 67 71 2 71 10 75 1 6 75 79 1 79 10 83 1 5
             83 87 2 10 87 91 1 6 91 95 1 9 95 99 1 99 9 103 2 103 10 107 1 5 107 111 1 9 111 115 2 13 115 119 1 119 10
             123 1 123 10 127 2 127 10 131 1 5 131 135 1 10 135 139 1 139 2 143 1 6 143 0 99 2 14 0 0])

(defn intcode-program
  ([v noun verb]
   (let [input (-> v (assoc 1 noun) (assoc 2 verb))]
     (->> [0 input]
          (iterate step)
          (remove first)
          first
          second)))
  ([v] (intcode-program v 12 2)))

(comment
  (->> [0 input]
       (iterate step)
       (take 4))

  (->> [0 input]
       (iterate step)
       (remove first)
       first
       second)

  ;5290681
  (first (intcode-program input1))

  ;{:noun 57, :verb 41, :solution 5741}
  (for [noun (range 100)
        verb (range 100)
        :let [res (first (intcode-program input1 noun verb))]
        :when (= res 19690720)]
    {:noun noun :verb verb :solution (+ (* 100 noun) verb)})

  )

;;;;;;;;;;;;;;;;;;;; Solution Using Multimethods ;;;;;;;;;;;;;;;;;;;;
(defmulti instruction (fn [[p v]] (v p)))

(defmethod instruction 1 [[p v]]
  (let [[op p0 p1 pos] (subvec v p)]
    [(+ p 4) (assoc v pos (+ (v p0) (v p1)))]))

(defmethod instruction 2 [[p v]]
  (let [[op p0 p1 pos] (subvec v p)]
    [(+ p 4) (assoc v pos (* (v p0) (v p1)))]))

(defmethod instruction 99 [[_ v]]
  [nil v])

(defmethod instruction :default [[p v]]
  [nil nil])

(defn program
  ([v noun verb]
   (let [input (-> v (assoc 1 noun) (assoc 2 verb))]
     (->> [0 input]
          (iterate instruction)
          (remove first)
          first
          second)))
  ([v] (program v 12 2)))

(comment
  (->> [0 input]
       (iterate instruction)
       (take 4))

  (->> [0 input]
       (iterate instruction)
       (remove first)
       first
       second)


  ;5290681
  (first (program input1))

  ;{:noun 57, :verb 41, :solution 5741}
  (for [noun (range 100)
        verb (range 100)
        :let [res (first (program input1 noun verb))]
        :when (= res 19690720)]
    {:noun noun :verb verb :solution (+ (* 100 noun) verb)})
  )



