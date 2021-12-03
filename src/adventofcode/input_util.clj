(ns adventofcode.input-util
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn read-as-array [res]
  (->> (io/resource res) slurp (format "[%s]") edn/read-string))

(defn read-as-strings [res]
  (->> (io/resource res) slurp str/split-lines))