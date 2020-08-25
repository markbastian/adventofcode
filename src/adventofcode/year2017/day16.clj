(ns adventofcode.year2017.day16
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def p "abcdefghijklmnop")

(defn spin [v x]
  (let [[a b] (split-at (- (count v) x) v)]
    (into (vec b) a)))

(comment
  (= [\c \d \e \a \b] (spin (seq "abcde") 3)))

(defn exchange [v a b]
  (-> v (assoc a (v b)) (assoc b (v a))))

(defn partner [v a b]
  (let [m (zipmap v (range))]
    (exchange v (m a) (m b))))

(comment
  (-> (vec "abcde")
      (spin 1)
      (exchange 3 4)
      (partner \e \b)
      cs/join))

(def input (-> "adventofcode/year2017/day16/input.txt"
               io/resource
               slurp
               (cs/split #",")))

(defn parse [s]
  (let [r (subs s 1)]
    (case (first s)
      \s #(spin % (Long/parseLong r))
      \x (let [[a b] (cs/split r #"/")]
           #(exchange % (Long/parseLong a) (Long/parseLong b)))
      \p (let [[[a] [b]] (cs/split r #"/")]
           #(partner % a b)))))

(defn dance-once [commands programs]
  (reduce (fn [v command] (command v)) programs commands))

(comment
  (time
    (= "kpbodeajhlicngmf"
       (cs/join (dance-once (map parse input) (vec p)))))

  (time
    (= "ahgpjdkcbfmneloi"
       (let [step (partial dance-once (map parse input))]
         (let [[f & r] (iterate step (vec p))
               s (vec (map cs/join (cons f (take-while (complement #{f}) r))))]
           (s (mod 1000000000 (count s))))))))
