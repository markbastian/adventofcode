(ns adventofcode.year2017.day13
  (:require [clojure.string :as cs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn create-scanner [scanner-range]
  (if (zero? scanner-range)
    {:scanner-pos nil :op identity}
    {:scanner-range scanner-range :scanner-pos 0 :op inc}))

(def ops-map {inc dec dec inc identity identity})

(defn adjust-op [{:keys [scanner-range scanner-pos op] :as scanner}]
  (if (or (nil? scanner-pos) (< -1 (op scanner-pos) scanner-range))
    scanner
    (update scanner :op ops-map)))

(defn scanner-step [scanner]
  (let [{:keys [op] :as scanner} (adjust-op scanner)]
    (update scanner :scanner-pos op)))

(defn parse-input [s]
  (letfn [(f [line] (mapv (comp edn/read-string cs/trim) (cs/split line #":")))]
    (mapv f (cs/split-lines s))))

(defn build-firewall [desc]
  (let [[scanner-range] (last desc)
        m (into {} desc)]
    (mapv (fn [i] (create-scanner (m i 0))) (range (inc scanner-range)))))

(defn firewall-seq [firewall]
  (iterate #(mapv scanner-step %) firewall))

(defn packet-run-seq [[firewall :as firewall-seq]]
  (->> firewall-seq
       (map #(mapv :scanner-pos %))
       (partition (count firewall) 1)
       (map (fn [s] (map #(%1 %2) s (range))))))

(defn severity [firewall-depths packet-run]
  (let [d (map-indexed (fn [i v] (if (zero? (or v ##NaN)) i 0)) packet-run)]
    (reduce + (map * firewall-depths d))))

(defn severity-seq [firewall]
  (let [firewall-depths (map (comp #(or % 0) :scanner-range) firewall)]
    (->> firewall
         firewall-seq
         packet-run-seq
         (map (partial severity firewall-depths)))))

(defn caught? [s]
  (true? (some zero? (filter identity s))))

(defn safe-steps [firewall]
  (reduce
    (fn [i packet-run] (if (caught? packet-run) (inc i) (reduced i)))
    0
    (->> firewall firewall-seq packet-run-seq)))

(def example-data "0: 3\n1: 2\n4: 4\n6: 4")
(def data (-> "adventofcode/year2017/day13/input.txt" io/resource slurp))

(comment
  (= 24 (-> example-data parse-input build-firewall severity-seq first))
  (= 1704 (-> data parse-input build-firewall severity-seq first))

  (= 10 (->> example-data parse-input build-firewall safe-steps))
  ;Note - this can take a very long time.
  (= 3970918 (->> data parse-input build-firewall safe-steps))
  )