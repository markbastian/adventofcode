(ns adventofcode.year2021.day16
  (:require [adventofcode.input-util :as iu]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def decoder (->> (iu/read-as-strings "adventofcode/year2021/day16/decoder.txt")
                  (map #(str/split % #" = "))
                  (reduce (fn [m [k v]] (assoc m (first k) v)) {})))
(def sample-input (slurp (io/resource "adventofcode/year2021/day16/sample-input.txt")))
(def input (slurp (io/resource "adventofcode/year2021/day16/input.txt")))

(defn b->l [s]
  (cond
    (string? s) (Long/parseLong s 2)
    (sequential? s) (recur (apply str s))))

(defn header [s]
  (let [[v r] (split-at 3 s)
        [t r] (split-at 3 r)]
    {:version (b->l v)
     :type-id    (b->l t)
     :r              r}))

(defn parse-literal [s]
  (loop [[group r] (split-at 5 s) n []]
    (cond
      (= \1 (first group))
      (recur (split-at 5 r) (into n (rest group)))
      (= \0 (first group))
      [(b->l (into n (rest group))) r])))

(defn parse-bit-length [s]
  (let [[group r] (split-at 15 s)]
    [(b->l group) r]))

(let [input "38006F45291200"
      ;input sample-input
      s (mapcat decoder input)]
  (let [{:keys [version type-id r] :as x} (header s)]
    (if (= 4 type-id)
      (parse-literal r)
      (let [[f & r] r]
        (case f
          \0 (let [[a r] (split-at 15 r)]
               [(b->l a) r])
          \1 "IDK"))
      #_
      (case version
        0 (parse-bit-length r)))))
