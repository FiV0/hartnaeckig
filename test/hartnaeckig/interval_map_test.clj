(ns hartnaeckig.interval-map-test
  (:require [clojure.test :refer [deftest is are]]
            [clojure.math.combinatorics :as combo]
            [hartnaeckig.interval-map :as interval-map]))

(def interval-data [[[0 1] 1] [[0 5] 2] [[1 3] 3] [[3 9] 4]
                    [[4 5] 5] [[4 7] 6] [[6 8] 7] [[8 9] 8] [[9 9] 9]])

(deftest seq-test
  (let [interval-data (butlast interval-data)]
    (doseq [p (combo/permutations interval-data)]
      (is (= (seq interval-data) (seq (apply interval-map/interval-map p)))))))

(deftest dissoc-test
  (let [i-map (apply interval-map/interval-map interval-data)]
    (doseq [[i _ :as kv] interval-data]
      (is (= (seq (remove #{kv} interval-data)) (seq (dissoc i-map i)))))))

(deftest assoc-test
  (doseq [[k v :as kv] interval-data]
    (is (= (seq interval-data) (seq (assoc (apply interval-map/interval-map (remove #{kv} interval-data)) k v))))))

(deftest empty-test
  (let [empty-set (interval-map/interval-map)]
    (is (= nil (first empty-set)))
    (is (= empty-set (rest empty-set)))
    (is (= nil (peek empty-set)))
    (is (= empty-set (pop empty-set)))))

(deftest hash-test
  (is (= (interval-map/interval-map) (interval-map/interval-map)))
  (is (not= (interval-map/interval-map) (interval-map/interval-map [[0 1] 1])))
  (is (= (interval-map/interval-map [[0 1] 1]) (interval-map/interval-map [[0 1] 1])))
  (is (= (apply interval-map/interval-map interval-data) (apply interval-map/interval-map (reverse interval-data)))))

(deftest meta-test
  (let [s1 (with-meta (interval-map/interval-map [[1 2] 1] [[0 5] 2] [[4 5] 3]) {:foo :bar})
        s2 (interval-map/interval-map [[1 2] 1] [[0 5] 2] [[4 5] 3])]
    (is (= s1 s2))
    (is (= {:foo :bar} (meta s1)))
    (is (= nil (meta s2)))))

#_(deftest nth-test)

(deftest get-test
  (is (= '(1 2 3 4 5 6) (seq (get (apply interval-map/interval-map interval-data) [0 5]))))
  (is (= '(2 4 5 6) (seq (get (apply interval-map/interval-map interval-data) [4 4]))))
  (is (= nil (seq (get (apply interval-map/interval-map interval-data) [-100 -1]))))
  (is (= nil (seq (get (apply interval-map/interval-map interval-data) [10 100])))))

(comment
  (clojure.test/run-tests))
