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
  (let [i-set (apply interval-set/interval-set interval-data)]
    (doseq [i interval-data]
      (is (= (seq (remove #{i} interval-data)) (seq (disj i-set i)))))))

(deftest conj-test
  (doseq [i interval-data]
    (is (= (seq interval-data) (seq (conj (apply interval-set/interval-set (remove #{i} interval-data)) i))))))

(deftest empty-test
  (let [empty-set (interval-set/interval-set)]
    (is (= nil (first empty-set)))
    (is (= empty-set (rest empty-set)))
    (is (= nil (peek empty-set)))
    (is (= empty-set (pop empty-set)))))

(deftest hash-test
  (is (= (interval-set/interval-set) (interval-set/interval-set)))
  (is (not= (interval-set/interval-set) (interval-set/interval-set [0 1])))
  (is (= (interval-set/interval-set [0 1]) (interval-set/interval-set [0 1])))
  (is (= (apply interval-set/interval-set interval-data) (apply interval-set/interval-set (reverse interval-data)))))

(deftest meta-test
  (let [s1 (with-meta (interval-set/interval-set [1 2] [0 5] [4 5]) {:foo :bar})
        s2 (interval-set/interval-set [1 2] [0 5] [4 5])]
    (is (= s1 s2))
    (is (= {:foo :bar} (meta s1)))
    (is (= nil (meta s2)))))

#_(deftest fn-set-test
    (is (= [1 2] ((interval-set/interval-set [1 2]) [1 2])))
    (is (= nil ((interval-set/interval-set [1 2]) [1 3]))))

#_(deftest nth-test)

(deftest get-test
  (is (= '([0 1] [0 5] [1 3] [3 9] [4 5] [4 7]) (seq (get (apply interval-set/interval-set interval-data) [0 5]))))
  (is (= '([0 5] [3 9] [4 5] [4 7]) (seq (get (apply interval-set/interval-set interval-data) [4 4]))))
  (is (= nil (seq (get (apply interval-set/interval-set interval-data) [-100 -1]))))
  (is (= nil (seq (get (apply interval-set/interval-set interval-data) [10 100])))))

(comment
  (clojure.test/run-tests))
