(ns adventofcode.input-util
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn read-as-array [res]
  (->> (io/resource res) slurp (format "[%s]") edn/read-string))