(ns hartnaeckig.interval-map
  (:require [hartnaeckig.interval-tree :as it]
            [clojure.data.finger-tree :as ft :refer [Measured]])
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet IPersistentMap ILookup
                         IPersistentStack IPersistentCollection Associative
                         Sorted Reversible Indexed Counted IHashEq)
           (clojure.data.finger_tree EmptyTree)
           (hartnaeckig.interval_tree Key-Prio-Measure)))

(def ^:private notfound (Object.))

(defn- check-entry [m [k v]]
  (and (contains? m k)
       (= v (get m k))))

(deftype IntervalMap [cmpr tree mdata]
  Object
  (equals [this x]
    (or (identical? this x) (and (instance? IntervalMap x) (it/seq-equals tree x))))
  (hashCode [_] (reduce + (map ft/hashcode tree)))
  IHashEq
  (hasheq [this]
    (ft/hash-unordered this))
  clojure.lang.IObj
  (meta [_] mdata)
  (withMeta [_ mdata] (IntervalMap. cmpr tree mdata))
  Seqable
  ;; return 'tree' instead of 'this' so that result will be Sequential
  (seq [this] (when (seq tree) #_(seq tree) (map (fn [[k v]] (clojure.lang.MapEntry. k v)) (seq tree))))
  IPersistentCollection
  (cons [this o]
    (.assoc this (nth o 0) (nth o 1)))
  (empty [_] (IntervalMap. cmpr (empty tree) mdata))
  (equiv [this x] (.equals this x)) ; TBD
  ISeq
  (first [_] (first tree))
  (more [_] (IntervalMap. cmpr (rest tree) mdata))
  (next [_] (when-let [t (next tree)] (IntervalMap. cmpr t mdata)))
  IPersistentStack
  (peek [_] (peek tree))
  (pop [_] (IntervalMap. cmpr (pop tree) mdata))
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
  ;; Counted

  IPersistentMap
  ;; counted part
  (count [_] (:len (ft/measured tree)))
  ;; iterable part
  ;; TODO
  ;; associative part
  (containsKey [this [low high :as k]]
    (if (empty? tree)
      false
      (let [[_ [[low-x high-x]] r] (ft/split-tree tree #(>= 0 (cmpr low (:key %))))]
        (if (zero? (cmpr low low-x))
          (if (zero? (cmpr high high-x))
            true
            (let [[_ [[_ high-x]] _] (ft/split-tree r (partial it/at-least high))]
              (if (and high-x (zero? (cmpr high high-x)))
                true
                false)))
          false))))

  (entryAt [this [low high :as k]]
    (if (empty? tree)
      nil
      (let [[_ [[low-x high-x] v] r] (ft/split-tree tree #(>= 0 (cmpr low (:key %))))]
        (if (zero? (cmpr low low-x))
          (if (zero? (cmpr high high-x))
            (clojure.lang.MapEntry/create k v)
            (let [[_ [[_ high-x] v] _] (ft/split-tree r (partial it/at-least high))]
              (if (and high-x (zero? (cmpr high high-x)))
                (clojure.lang.MapEntry/create k v)
                nil)))
          nil))))

  (assoc [this [low high :as k] v]
    (let [entry [k v]]
      (if (empty? tree)
        (IntervalMap. cmpr (conj tree entry) mdata)
        (let [[l [[low-x high-x] :as x] r] (ft/split-tree tree #(>= 0 (cmpr low (:key %))))
              compared (cmpr low low-x)]
          (if (zero? compared)
            (let [compared2 (cmpr high high-x)]
              (if (zero? compared2)
                (IntervalMap. cmpr (ft/ft-concat (conj l [k v]) r) mdata)
                (let [[a b] (if (>= 0 compared2) [entry x] [x entry])]
                  (IntervalMap. cmpr (ft/ft-concat (conj l a) (ft/conjl r b)) mdata))))
            (let [[a b] (if (>= 0 compared) [entry x] [x entry])]
              (IntervalMap. cmpr (ft/ft-concat (conj l a) (ft/conjl r b)) mdata)))))))

  (assocEx [this k v] (throw (Exception.)))

  (without [this [low high :as k]]
    (if (empty? tree)
      this
      (let [[l [[low-x high-x :as x]] r] (ft/split-tree tree #(>= 0 (cmpr low (:key %))))]
        (if (zero? (cmpr low low-x))
          (if (zero? (cmpr high high-x))
            (IntervalMap. cmpr (ft/ft-concat l r) mdata)
            (let [[l2 [[_ high-x]] r2] (ft/split-tree r (partial it/at-least high))]
              (if (and high-x (zero? (cmpr high high-x)))
                (IntervalMap. cmpr (ft/ft-concat (conj l x) (ft/ft-concat l2 r2)) mdata)
                this)))
          this))))

  ILookup
  (valAt [_ [l h] notfound]
    (if (empty? tree)
      notfound
      (letfn [(matches [tree]
                (let [new-tree (it/drop-until (partial it/at-least l) tree)]
                  (if-let [x (first new-tree)]
                    (cons (second x) (matches (rest new-tree)))
                    nil)))]
        (matches (it/take-until (partial it/greater h) tree)))))
  (valAt [this k]
    (.valAt this k nil))

  ;; clojure.lang.ISeq
  ;; (seq [this]
  ;;   (seq
  ;;    (map (fn [[k v]] (clojure.lang.MapEntry. k (get this k))) m)))

  ;; TODO
  ;; java.util.Map
  ;; (contains [this x] (not= notfound (get this x notfound)))
  ;; (containsAll [this xs] (every? #(contains? this %) xs))
  ;; (isEmpty [_] (empty? tree))
  java.lang.Iterable
  (iterator [_]
    (let [t (atom tree)]
      (reify java.util.Iterator
        (next [_] (let [f (first @t)]
                    (swap! t next)
                    f))
        (hasNext [_] (boolean (first @t))))))
  ;; (size [this] (count this))
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

(let [measure-kp (fn [[[l h] _v]] (Key-Prio-Measure. l h))
      zero-kp (Key-Prio-Measure. nil Long/MIN_VALUE)
      meter-kp (ft/meter measure-kp
                         zero-kp
                         (fn [{key1 :key prio1 :prio} {key2 :key prio2 :prio}]
                           (Key-Prio-Measure. (or key2 key1) (max prio1 prio2))))
      empty-tree (EmptyTree. meter-kp)
      default-empty-interval-set (IntervalMap. compare empty-tree nil)]
  (defn interval-map [& args]
    (into default-empty-interval-set args)))

(prefer-method clojure.pprint/simple-dispatch clojure.lang.IPersistentMap clojure.lang.ISeq)


(comment
  (def is (interval-map [[0 1] 1]
                        [[1 3] 2]
                        [[4 7] 3]
                        [[8 9] 4]
                        [[0 5] 5]
                        [[6 8] 6]
                        [[9 9] 7]
                        [[3 9] 8]
                        [[4 5] 9]))

  (seq is)
  (keys is)
  (vals is)

  (get is [1 2])
  (get is [1 3])
  (get is [4 9])

  (is [1 2])
  (is [-1 -1])

  (assoc is [-2 15] 111)
  (assoc is [0 1] 111)
  (dissoc  (assoc is [-2 15] 111) [-2 15])

  (meta (dissoc  (assoc is [-2 15] 111) [-2 15]))
  (meta (with-meta (dissoc  (assoc is [-2 15] 111) [-2 15]) {:a 1}))

  (assoc (empty (meta (with-meta (dissoc  (assoc is [-2 15] 111) [-2 15]) {:a 1})))
         [1 2] 'foo)

  (find is [0 1]))
