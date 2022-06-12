(ns hartnaeckig.interval-tree
  (:require [clojure.data.finger-tree :as ft]))

(defrecord Key-Prio-Measure [key prio])

(def measure-kp (fn [[l h]] (Key-Prio-Measure. l h)))
(def zero-kp (Key-Prio-Measure. (Long/MAX_VALUE) (Long/MIN_VALUE)))
(def meter-kp (ft/meter measure-kp
                        zero-kp
                        (fn [{key1 :key prio1 :prio} {key2 :key prio2 :prio}]
                          (Key-Prio-Measure. (min key1 key2) (max prio1 prio2)))))

(def empty-interval-tree (ft/finger-tree meter-kp))

(def it (conj empty-interval-tree
              [1 3]
              [4 7]
              [8 9]
              [0 5]
              [6 8]
              [9 9]
              [3 9]
              [4 5]))

(ft/measured it)

(defn at-least [r {:keys [prio]}]
  (<= r prio))

(defn greater [r {:keys [key]}]
  (< r key))

(defn some-interval [it [l h]]
  (let [[_ [low high :as x] _] (ft/split-tree it (partial at-least l))]
    (if (and (at-least l (ft/measured it))
             (<= low h))
      x
      nil)))

(some-interval it [1 1])
(some-interval it [4 4])
(some-interval it [9 9])
