(ns hartnaeckig.interval-tree
  (:require [clojure.data.finger-tree :as ft :refer [Measured SplitAt]])
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet ILookup
                         IPersistentStack IPersistentCollection Associative
                         Sorted Reversible Indexed Counted IHashEq)
           (clojure.data.finger_tree EmptyTree)))

(defrecord Key-Prio-Measure [key prio])

(defn- at-least [r {:keys [prio]}]
  (<= r prio))

(defn- greater [r {:keys [key]}]
  (< r key))

(def ^:private notfound (Object.))

(defn- seq-equals [a b]
  (boolean
   (when (or (sequential? b) (instance? java.util.List b))
     (loop [a (seq a), b (seq b)]
       (when (= (nil? a) (nil? b))
         (or
          (nil? a)
          (when (= (first a) (first b))
            (recur (next a) (next b)))))))))

(def measure-kp (fn [[l h]] (Key-Prio-Measure. l h)))
(def zero-kp (Key-Prio-Measure. nil Long/MIN_VALUE))
(def meter-kp (ft/meter measure-kp
                        zero-kp
                        (fn [{key1 :key prio1 :prio} {key2 :key prio2 :prio}]
                          (Key-Prio-Measure. (or key2 key1) (max prio1 prio2)))))

(deftype IntervalSet [cmpr tree mdata]
  Object
  (equals [_ x]
    (boolean
     (if (instance? java.util.Set x)
       (and (= (count x) (count tree))
            (every? #(contains? x %) tree))
       (seq-equals tree x))))
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
      ;; get one interval implementation
      (let [[_ [low high :as x] _] (ft/split-tree tree (partial at-least l))]
        (if (and (at-least l (ft/measured tree))
                 (<= low h))
          x
          notfound))
      ;; old ordered set implementation
      #_(let [x (second (ft/split-tree tree #(>= 0 (cmpr k (:right %)))))]
          (if (= x k)
            k
            notfound))))
  (valAt [this k]
    (.valAt this k nil))
  IPersistentSet
  (disjoin [this [low high :as i]]
    (if (empty? tree)
      this
      (let [[left [low-x high-x :as x] right] (ft/split-tree tree #(<= low (:key %)) #_#(>= 0 (cmpr l (:right %))))]
        (println 1 x)
        (cond
          (= x i) ; found the exact match
          (IntervalSet. cmpr (ft/ft-concat left right) mdata)
          (let [measured (ft/measured right)]
            (and (= low (:key measured)) (<= high (:prio measured)))) ; more searching in r needed
          (let [[left-y y right-y] (ft/split-tree right (partial at-least high))]
            (println 2 y)
            (if (= y i)
              (IntervalSet. cmpr (ft/ft-concat (conj left x) (ft/ft-concat left-y right-y)) mdata)
              this))
          :else this))))
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
  )

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

  (disj is [1 3])

  (disj (interval-set [0 5] [0 2] [0 3] [0 1]) [0 1])

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
