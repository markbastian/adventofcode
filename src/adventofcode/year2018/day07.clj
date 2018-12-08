(ns adventofcode.year2018.day07
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]
            [clojure.set :as s]))

(def input
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(def rgx #"Step ([A-Z]) must be finished before step ([A-Z]) can begin\.")

(defn step [{:keys [steps order]}]
  (let [blockers (set (map second steps))
        [[f to]] (->> steps (remove (fn [[f]] (blockers f))) (sort-by first))
        remaining-steps (set (remove (fn [[a]] (= a f)) steps))]
    {:steps (seq (cond-> remaining-steps to (conj [to])))
     :order (str order f)}))

(defn parse [input]
  (->> input
       (re-seq rgx)
       (map (fn [[_ f t]] [f t]))))

(defn order [input]
  (->> {:steps (parse input) :order ""}
       (iterate step)
       (drop-while :steps)
       first
       :order))

(comment
  ;"CABDFE"
  (order input)
  ;"AHJDBEMNFQUPVXGCTYLWZKSROI"
  (->> "adventofcode/year2018/day07/input.txt" io/resource slurp order))

(defn job-map [input]
  (letfn [(add-job [m [k v]] (-> m (update k conj v) (update v identity)))]
    (reduce add-job {} (parse input))))

(defn duration [fixed-job-cost job]
  (+ fixed-job-cost (inc (apply - (map int (str job "A"))))))

(defn job-status [{:keys [progress duration]}]
  (if (>= progress duration) :complete :incomplete))

(defn complete-jobs [{:keys [job-queue active-jobs] :as m}]
  (let [{:keys [complete incomplete]} (group-by job-status active-jobs)
        complete-ids (reduce dissoc job-queue (map :job complete))]
    (-> m
        (assoc :active-jobs incomplete)
        (update :idle-workers into (map :worker complete))
        (assoc :job-queue complete-ids))))

(defn advance-time [m]
  (letfn [(advance-job [job] (update job :progress inc))]
    (update m :active-jobs #(map advance-job %))))

(defn assign-work [fixed-job-cost {:keys [job-queue active-jobs idle-workers] :as m}]
  (letfn [(create-job [w j] (zipmap [:worker :job :progress :duration] [w j 0 (duration fixed-job-cost j)]))]
    (let [blocked-jobs (distinct (mapcat second job-queue))
          current-jobs (set (map :job active-jobs))
          unblocked-jobs (remove current-jobs (keys (reduce #(dissoc %1 %2) job-queue blocked-jobs)))
          new-jobs (map create-job idle-workers unblocked-jobs)]
      (-> m
          (update :active-jobs into new-jobs)
          (update :idle-workers s/difference (set (map :worker new-jobs)))))))

(defn work-step [fixed-job-cost m]
  (->> m
       advance-time
       complete-jobs
       (assign-work fixed-job-cost)))

(defn job-seq [fixed-job-cost n-workers input]
  (->> {:job-queue (job-map input) :idle-workers (set (range n-workers))}
       (iterate (partial work-step fixed-job-cost))
       rest
       (map (fn [{:keys [active-jobs]}] (map :job active-jobs)))
       (take-while seq)))

(defn job-duration [fixed-job-cost n-workers input]
  (count (job-seq fixed-job-cost n-workers input)))

(comment
  ;15
  (job-duration 0 2 input)
  ;1031
  (->> "adventofcode/year2018/day07/input.txt"
       io/resource
       slurp
       (job-duration 60 5)))