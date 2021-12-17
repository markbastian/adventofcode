(ns adventofcode.year2021.day16
  (:require [adventofcode.input-util :as iu]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]))

(def decoder (->> (iu/read-as-strings "adventofcode/year2021/day16/decoder.txt")
                  (map #(str/split % #" = "))
                  (reduce (fn [m [k v]] (assoc m (first k) v)) {})))
(def input (slurp (io/resource "adventofcode/year2021/day16/input.txt")))

(defn b->l [s]
  (cond
    (string? s) (Long/parseLong s 2)
    (sequential? s) (recur (apply str s))))

(defn take-n [c n]
  (vec (repeatedly n #(async/<!! c))))

(defn read-n [c n]
  (b->l (repeatedly n #(async/<!! c))))

(defn parse-literal [s]
  (loop [[f & r] (take-n s 5) n []]
    (cond
      (= \1 f)
      (recur (take-n s 5) (into n r))
      (= \0 f) (b->l (into n r)))))

(defn parse-packet [c]
  (let [[f :as version-bits] (take-n c 3)]
    (when f
      (let [version (b->l version-bits)
            type-id (read-n c 3)]
        [version type-id
         (case type-id
           4 (parse-literal c)
           (let [f (async/<!! c)]
             (case f
               \0 (let [length-in-bits (read-n c 15)
                        block (async/to-chan!! (take-n c length-in-bits))]
                    (loop [packet (parse-packet block) packets []]
                      (if packet
                        (recur (parse-packet block) (conj packets packet))
                        packets)))
               \1 (let [num-contained-packets (read-n c 11)]
                    (vec (repeatedly num-contained-packets #(parse-packet c)))))))]))))

(defn parse-packet-hex-string [input]
  (parse-packet (async/to-chan!! (mapcat decoder input))))

(defn version-sum [[version _type-id value]]
  (if (number? value)
    version
    (reduce + version (map version-sum value))))

(def part1 (comp version-sum parse-packet-hex-string))

(defn apply-operations [[_version type-id value]]
  (case type-id
    0 (apply + (map apply-operations value))
    1 (apply * (map apply-operations value))
    2 (apply min (map apply-operations value))
    3 (apply max (map apply-operations value))
    4 value
    5 (let [[a b] (map apply-operations value)] (if (> a b) 1 0))
    6 (let [[a b] (map apply-operations value)] (if (< a b) 1 0))
    7 (let [[a b] (map apply-operations value)] (if (= a b) 1 0))))

(def part2 (comp apply-operations parse-packet-hex-string))

(comment
  ;; Part 1
  (parse-packet-hex-string "D2FE28")
  (parse-packet-hex-string "38006F45291200")
  (parse-packet-hex-string "EE00D40C823060")
  (= 16 (part1 "8A004A801A8002F478"))
  (= 12 (part1 "620080001611562C8802118E34"))
  (= 23 (part1 "C0015000016115A2E0802F182340"))
  (= 31 (part1 "A0016C880162017C3686B18A3D4780"))
  (= 875 (part1 input))

  ;; Part 2
  (= 2021 (part2 "D2FE28"))
  (= 3 (part2 "C200B40A82"))
  (= 54 (part2 "04005AC33890"))
  (= 7 (part2 "880086C3E88112"))
  (= 9 (part2 "CE00C43D881120"))
  (= 1 (part2 "D8005AC2A8F0"))
  (= 0 (part2 "F600BC2D8F"))
  (= 0 (part2 "9C005AC2F8F0"))
  (= 1 (part2 "9C0141080250320F1802104A08"))
  (= 1264857437203 (part2 input))

  )