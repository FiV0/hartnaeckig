(ns hartnaeckig.interval-tree
  (:require [clojure.data.finger-tree :as ft :refer [Measured SplitAt]])
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet ILookup
                         IPersistentStack IPersistentCollection Associative
                         Sorted Reversible Indexed Counted IHashEq)
           (clojure.data.finger_tree EmptyTree)))

(defrecord Key-Prio-Measure [key prio])

(defn at-least [r {:keys [prio]}]
  (<= r prio))

(defn greater [r {:keys [key]}]
  (< r key))

(defn seq-equals [a b]
  (boolean
   (when (or (seq? b) (instance? java.util.List b))
     (loop [a (seq a), b (seq b)]
       (when (= (nil? a) (nil? b))
         (or
          (nil? a)
          (when (= (first a) (first b))
            (recur (next a) (next b)))))))))

(defn split [p tree]
  (if (p (ft/measured tree))
    (let [[l x r] (ft/split-tree tree p)]
      [l (ft/conjl r x)])
    [tree nil]))

(defn take-until [p tree]
  (first (split p tree)))

(defn drop-until [p tree]
  (second (split p tree)))
