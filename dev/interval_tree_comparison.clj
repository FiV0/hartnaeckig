(ns interval-tree-comparison
  (:require [clj-async-profiler.core :as prof]
            [com.dean.interval-tree.core :as dean]
            [criterium.core :as crit]
            [hartnaeckig.interval-map :as interval-map]
            [hartnaeckig.interval-set :as interval-set]))

(defn random-seq [n] (shuffle (range n)))

(def foo (->> (map vector (random-seq 500000) (random-seq 500000))
              (map (fn [[x y]] (if (> x y) [y x] [x y])))
              doall))

(def bar (->> (map vector (random-seq 500000) (random-seq 500000))
              (map (fn [[x y]] (if (> x y) [y x] [x y])))
              doall))

(def d-is (time (dean/interval-set foo)))
(def h-is (prof/profile (apply interval-set/interval-set foo)))

(time (reduce disj d-is foo))
(time (reduce disj h-is foo))

(time (def res1 (doall (map #(get d-is %) (take 100 bar)))))
(time (def res2 (doall (map #(get h-is %) (take 100 bar)))))

(def foo-map (->> (map vector (random-seq 500000) (random-seq 500000))
                  (map (fn [[x y]] (if (> x y) [y x] [x y])))
                  (map-indexed (fn [k v] [v k]))
                  doall))

(def d-im (time (dean/interval-map foo-map)))
(def h-im (time (apply interval-map/interval-map foo-map)))

(def res (reduce dissoc d-im (map first foo-map)))

(time (reduce dissoc d-im (map first foo-map)))
(time (reduce dissoc h-im (map first foo-map)))

(time (def res1 (doall (map #(get d-is %) (take 100 bar)))))
(time (def res2 (doall (map #(get h-is %) (take 100 bar)))))

(comment
  (prof/serve-files 8080)

  )
