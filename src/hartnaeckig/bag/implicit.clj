(ns hartnaeckig.bag.implicit)

(deftype Bag [size content]
  clojure.lang.IPersistentSet

  (seq [this] (seq content))

  (count [this] size)

  (cons [this o]
    (Bag. (inc size) (update content o (fnil + 0) 1)))

  (empty [this] (Bag. 0 {}))

  (equiv [this other] (= this other))

  (disjoin [this key]
    (let [cnt (get content key)]
      (Bag. (dec size)
            (cond
              (nil? cnt) (throw (ex-info "No such key" {:key key}))
              (= cnt 1) (dissoc content key)
              :else (update content key - 1)))))

  (contains [this key] (some? (get content key)))

  (get [this key] (get content key))

  clojure.lang.IFn
  (invoke [this k]
    (get this k))

  (applyTo [this args]
    (let [n (clojure.lang.RT/boundedLength args 2)]
      (if (= 1 n)
        (.invoke this (first args))
        (throw (clojure.lang.ArityException. n (.. this (getClass) (getSimpleName))))))))

(defn bag [] (Bag. 0 {}))

(comment
  (def b (-> (bag) (conj "foo") (conj "foo") #_(disj "foo") (conj "bar")))
  (b "foo") ;; => 2
  (count b)
  (get b "foo")
  (contains? b "foo")
  (seq b))
