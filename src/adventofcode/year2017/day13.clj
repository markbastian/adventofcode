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
  (->> s
       cs/split-lines
       (mapv (fn [line]
               (->> (cs/split line #":")
                    (mapv (comp edn/read-string cs/trim)))))))

(defn build-firewall [desc]
  (let [[scanner-range] (last desc)
        m (into {} desc)]
    (mapv (fn [i] (create-scanner (m i 0))) (range (inc scanner-range)))))

(defn firewall-seq [firewall]
  (->> firewall
       (iterate #(mapv scanner-step %))
       (map (fn [fw] (mapv (fn [scanner] (dissoc scanner :op)) fw)))))

(defn severity [[f :as firewall-seq]]
  (->> (map vector (range (count f)) firewall-seq)
       (map (fn [[packet-pos fw]]
              (let [{:keys [scanner-pos scanner-range]} (fw packet-pos)]
                (if (= 0 scanner-pos)
                  (* packet-pos scanner-range)
                  0))))
       (reduce +)))

(defn severity-seq [input]
  (let [firewall (build-firewall input)]
    (map severity (iterate rest (firewall-seq firewall)))))

(def example-data "0: 3\n1: 2\n4: 4\n6: 4")
(def data (-> "adventofcode/year2017/day13/input.txt" io/resource slurp))

(defn safe-steps [firewall]
  (->> firewall
       firewall-seq
       (map #(mapv :scanner-pos %))
       (partition (count firewall) 1)
       (map (fn [s] (map #(%1 %2) s (range))))
       (map (fn [s] (some zero? (filter identity s))))
       (take-while true?)
       count))

(comment
  (= 24 (-> example-data parse-input severity-seq first))
  (= 1704 (-> data parse-input severity-seq first))

  (= 10 (->> example-data parse-input build-firewall safe-steps))
  ;Note - this can take a very long time.
  (= 3970918 (->> data parse-input build-firewall safe-steps))
  )