(ns adventofcode.year2020.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn parse-input [input]
  (->> (io/resource input)
       slurp
       cs/split-lines
       (partition-by (comp some? seq))
       (remove #{[""]})
       (map (partial cs/join "\n"))
       (map (partial re-seq #"(\p{Alpha}{3}):([\S]+)"))
       (map (fn [s] (into {} (map (comp vec rest) s))))))

(def test-input (parse-input "adventofcode/year2020/day04/test-input.txt"))
(def input (parse-input "adventofcode/year2020/day04/input.txt"))

(def required-fields ["byr" "ecl" "eyr" "hcl" "hgt" "iyr" "pid"])

(defn part1 [input]
  (->> input
       (map (fn [m] (select-keys m required-fields)))
       (filter (fn [m] (= (count required-fields) (count m))))
       count))

(comment
  (time (= 2 (part1 test-input)))
  (time (= 239 (part1 input))))

(defmulti valid? first)
(defmethod valid? "byr" [[_ v]]
  (and (re-matches #"\d{4}" v) (<= 1920 (Long/parseLong v) 2002)))
(defmethod valid? "iyr" [[_ v]]
  (and (re-matches #"\d{4}" v) (<= 2010 (Long/parseLong v) 2020)))
(defmethod valid? "eyr" [[_ v]]
  (and (re-matches #"\d{4}" v) (<= 2020 (Long/parseLong v) 2030)))
(defmethod valid? "hgt" [[_ v]]
  (let [[_ h u] (re-matches #"(\d+)(cm|in)" v)]
    (case u
      "cm" (<= 150 (Long/parseLong h) 193)
      "in" (<= 59 (Long/parseLong h) 76)
      false)))
(defmethod valid? "hcl" [[_ v]]
  (re-matches #"#[a-f0-9]{6}" v))
(defmethod valid? "ecl" [[_ v]]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v))
(defmethod valid? "pid" [[_ v]] (re-matches #"\d{9}" v))
(defmethod valid? "cid" [[_ v]] true)

(defn part2 [input]
  (->> input
       (map (fn [m] (select-keys m required-fields)))
       (map (fn [m] (filter valid? m)))
       (filter (fn [m] (= (count required-fields) (count m))))
       count))

(comment
  (time (= 0 (part2 (parse-input "adventofcode/year2020/day04/invalid.txt"))))
  (time (= 4 (part2 (parse-input "adventofcode/year2020/day04/valid.txt"))))
  (time (= 2 (part2 test-input)))
  (time (= 188 (part2 input))))


