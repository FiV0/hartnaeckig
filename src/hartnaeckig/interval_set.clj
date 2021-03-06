(ns hartnaeckig.interval-set
  (:require [hartnaeckig.interval-tree :as it]
            [clojure.data.finger-tree :as ft :refer [Measured]])
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet IPersistentMap ILookup
                         IPersistentStack IPersistentCollection Associative
                         Sorted Reversible Indexed Counted IHashEq)
           (clojure.data.finger_tree EmptyTree)
           (hartnaeckig.interval_tree Key-Prio-Measure)))

(def ^:private notfound (Object.))

;; TODO try to fix printing of sets
;; TODO add assertion if input data is not a pair

(deftype IntervalSet [cmpr tree mdata]
  Object
  (equals [this x]
    (or (identical? this x) (and (instance? IntervalSet x) (it/seq-equals tree x))))
  (hashCode [_] (reduce + (map ft/hashcode tree)))
  IHashEq
  (hasheq [this]
    (ft/hash-unordered this))
  clojure.lang.IObj
  (meta [_] mdata)
  (withMeta [_ mdata] (IntervalSet. cmpr tree mdata))
  Seqable
  ;; return 'tree' instead of 'this' so that result will be Sequential
  (seq [this] (when (seq tree) tree))
  IPersistentCollection
  (cons [this [low high :as value]]
    (if (empty? tree)
      (IntervalSet. cmpr (conj tree value) mdata)
      (let [[l [low-x high-x :as x] r] (ft/split-tree tree #(>= 0 (cmpr low (:key %))))
            compared (cmpr low low-x)]
        (if (zero? compared)
          (let [compared2 (cmpr high high-x)]
            (if (zero? compared2)
              this ;; already in set
              (let [[a b] (if (>= 0 compared2) [value x] [x value])]
                (IntervalSet. cmpr (ft/ft-concat (conj l a) (ft/conjl r b)) mdata))))
          (let [[a b] (if (>= 0 compared) [value x] [x value])]
            (IntervalSet. cmpr (ft/ft-concat (conj l a) (ft/conjl r b)) mdata))))))
  (empty [_] (IntervalSet. cmpr (empty tree) mdata))
  (equiv [this x] (.equals this x)) ; TBD
  ISeq
  (first [_] (first tree))
  (more [_] (IntervalSet. cmpr (rest tree) mdata))
  (next [_] (when-let [t (next tree)] (IntervalSet. cmpr t mdata)))
  IPersistentStack
  (peek [_] (peek tree))
  (pop [_] (IntervalSet. cmpr (pop tree) mdata))
  Reversible
  (rseq [_] (rseq tree)) ; not 'this' because tree ops can't be reversed
  Measured
  (measured [_] (ft/measured tree))
  (getMeter [_] (ft/getMeter tree)) ; not needed?
  #_#_#_ ;; todo
  SplitAt
  (ft-split-at [this n notfound]
    (cond
      (< n 0) [(empty this) notfound this]
      (< n (count this)) (let [[l x r] (ft/split-tree tree #(> (:len %) n))]
                           [(IntervalSet. cmpr l mdata) x
                            (IntervalSet. cmpr r mdata)])
      :else [this notfound (empty this)]))
  (ft-split-at [this n]
    (ft/ft-split-at this n nil))
  Counted
  (count [_] (:len (ft/measured tree)))
  ILookup
  (valAt [_ [l h] notfound]
    (if (empty? tree)
      notfound
      (letfn [(matches [tree]
                (loop [tree tree res []]
                  (let [new-tree (it/drop-until (partial it/at-least l) tree)]
                    (if-let [x (first new-tree)]
                      (recur (rest new-tree) (conj res x))
                      res))))]
        (matches (it/take-until (partial it/greater h) tree)))

      ;; get one interval implementation
      #_(let [[_ [low high :as x] _] (ft/split-tree tree (partial at-least l))]
          (if (and (at-least l (ft/measured tree))
                   (<= low h))
            x
            notfound))))
  (valAt [this k]
    (.valAt this k nil))
  IPersistentSet
  (disjoin [this [low high :as i]]
    (if (empty? tree)
      this
      (let [[l [low-x high-x :as x] r] (ft/split-tree tree #(>= 0 (cmpr low (:key %))))]
        (if (zero? (cmpr low low-x))
          (if (zero? (cmpr high high-x))
            (IntervalSet. cmpr (ft/ft-concat l r) mdata)
            (let [[l2 [_ high-x] r2] (ft/split-tree r (partial it/at-least high))]
              (if (and high-x (zero? (cmpr high high-x)))
                (IntervalSet. cmpr (ft/ft-concat (conj l x) (ft/ft-concat l2 r2)) mdata)
                this)))
          this))))
  (get [this k] (.valAt this k nil))
  #_#_#_ ;; should this be added ?
  Indexed
  (nth [this n notfound] (if (< -1 n (:len (ft/measured tree)))
                           (second (ft/split-tree tree #(> (:len %) n)))
                           notfound))
  (nth [this n] (if (< -1 n (:len (ft/measured tree)))
                  (second (ft/split-tree tree #(> (:len %) n)))
                  (throw (IndexOutOfBoundsException.))))
  #_#_#_#_#_ ;; the interval tree is sorted, so this should be possible to implement
  Sorted
  (comparator [_] cmpr)
  (entryKey [_ x] x)
  (seq [this ascending?] (if ascending?  (.seq this) (rseq tree)))
  (seqFrom [_ k ascending?]
    (let [[l x r] (ft/split-tree tree #(>= 0 (cmpr k (:key %))))]
      (if ascending?
        (IntervalSet. cmpr (ft/conjl r x) mdata)
        (rseq (conj l x)))))
  java.util.Set
  (contains [this x] (not= notfound (get this x notfound)))
  (containsAll [this xs] (every? #(contains? this %) xs))
  (isEmpty [_] (empty? tree))
  (iterator [_]
    (let [t (atom tree)]
      (reify java.util.Iterator
        (next [_] (let [f (first @t)]
                    (swap! t next)
                    f))
        (hasNext [_] (boolean (first @t))))))
  (size [this] (count this))
  ;;toArray ... TBD

  clojure.lang.IFn
  (invoke [this k not-found]
    (.valAt this k not-found))
  (invoke [this k]
    (.valAt this k))
  (applyTo [this args]
    (let [n (clojure.lang.RT/boundedLength args 2)]
      (case n
        0 (throw (clojure.lang.ArityException. n (.. this (getClass) (getSimpleName))))
        1 (.invoke this (first args))
        2 (.invoke this (first args) (second args))
        3 (throw (clojure.lang.ArityException. n (.. this (getClass) (getSimpleName))))))))

(let [measure-kp (fn [[l h]] (Key-Prio-Measure. l h))
      zero-kp (Key-Prio-Measure. nil Long/MIN_VALUE)
      meter-kp (ft/meter measure-kp
                         zero-kp
                         (fn [{key1 :key prio1 :prio} {key2 :key prio2 :prio}]
                           (Key-Prio-Measure. (or key2 key1) (max prio1 prio2))))
      empty-tree (EmptyTree. meter-kp)
      default-empty-interval-set (IntervalSet. compare empty-tree nil)]
  (defn interval-set [& args]
    (into default-empty-interval-set args)))

(prefer-method clojure.pprint/simple-dispatch clojure.lang.IPersistentSet clojure.lang.ISeq)


(comment
  (def is (interval-set [0 1]
                        [1 3]
                        [4 7]
                        [8 9]
                        [0 5]
                        [6 8]
                        [9 9]
                        [3 9]
                        [4 5]))

  (def is2 (interval-set [1 3] [3 9] [4 5] [4 7] [6 8] [8 9] [9 9]))



  (disj is [1 3])
  (disj is [4 5])
  (disj is [10 10])

  (get is [1 2])
  (get is [1 3])
  (get is [4 9])

  (is [1 2])
  (is [-1 -1])
  (disj (interval-set [0 5] [0 2] [0 3] [0 1]) [0 5])

  (interval-set [0 1]
                [1 3]
                [4 7]
                [8 9]
                [0 5]
                [6 8]
                [9 9]
                [3 9]
                [4 5])

  )
