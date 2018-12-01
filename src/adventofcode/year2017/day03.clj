(ns adventofcode.year2017.day03)

(def dirs [[0 -1] [1 0] [0 1] [-1 0]])

(def dir-map (zipmap dirs (drop 1 (cycle dirs))))

(defn step [{:keys [v loc dir]}]
  (let [l (mapv + loc (dir-map dir))
        n (mapv + loc dir)]
    (if (v l)
      {:v (conj v n) :loc n :dir dir}
      {:v (conj v l) :loc l :dir (dir-map dir)})))

(def spiral-seq
  (->> {:v #{[0 0]} :loc [0 0] :dir (first dirs)} (iterate step) (map :loc)))

(defn spiral-memory-dist-1 [input]
  (->> (range)
       (map inc)
       (map vector spiral-seq)
       (some (fn [[[x y] v]] (when (= v input) (+ (Math/abs x) (Math/abs y)))))))

(comment
  ;326
  (time (spiral-memory-dist-1 361527)))

(defn neighbors [[i j]]
  (let [x ((juxt inc inc identity dec dec dec identity inc) i)
        y ((juxt identity inc inc inc identity dec dec dec) j)]
    (map vector x y)))

(defn spiral-memory-dist-2 [input]
  (->> (reductions
         (fn [{:keys [g]} c]
           (let [v (reduce + (map #(g % 0) (neighbors c)))]
             {:g (assoc g c v) :l c :v v}))
         {:g {[0 0] 1} :l [0 0] :v 1}
         (rest spiral-seq))
       (map :v)
       (filter #(> % input))
       first))

(comment
  ;363010
  (time (spiral-memory-dist-2 361527)))