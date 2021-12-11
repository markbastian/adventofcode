(ns adventofcode.misc.core
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [tick.core :as t]
            [tick.alpha.interval :as t.i])
  (:import (java.util Date)))

(defn enhance-ts [m]
  (reduce (fn [m [k v]]
            (let [ks (name k)]
              (if (str/ends-with? ks "-ts")
                (let [pfx (subs ks 0 (- (count ks) 2))]
                  (assoc m
                    k v
                    ))
                (assoc m k v))))
          {}
          m))

(defn normalize-leaderboard [leaderboard]
  (->> leaderboard
       (walk/prewalk
         (fn [x]
           (cond-> x
                   (map? x)
                   (-> (update-keys (fn [k]
                                      (if
                                        (re-matches #"\d+" k)
                                        (parse-long k)
                                        (keyword (str/replace k #"_" "-")))))
                       enhance-ts))))
       :members
       vals
       ))

(def leaderboard
  (->> "leaderboard.json"
       slurp
       json/read-str
       normalize-leaderboard))

