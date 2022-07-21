(ns hartnaeckig.interval-set-test
  (:require [clojure.test :refer [deftest is are]]
            [clojure.math.combinatorics :as combo]
            [hartnaeckig.interval-set :as interval-set]))

(def interval-data [[0 1] [0 5] [1 3] [3 9]
                    [4 5] [4 7] [6 8] [8 9] [9 9]])

(deftest seq-test
  (let [interval-data [[0 1] [0 5] [1 3] [3 9] [4 5] [4 7] [6 8] [8 9] #_[9 9]]]
    (doseq [p (combo/permutations interval-data)]
      (is (= (seq interval-data) (seq (apply interval-set/interval-set p)))))))

(deftest disj-test
  (let [is (apply interval-set/interval-set interval-data)]
    ;; is
    (doseq [i (take 1 interval-data)]
      #_(is (= 1 #_(seq (remove #{i} interval-data)) 1 #_(seq (disj is i)))))))

(disj-test)

(disj (apply interval-set/interval-set interval-data) (first interval-data))

(remove #{(first interval-data)} interval-data)

(comment
  (clojure.test/run-tests))
