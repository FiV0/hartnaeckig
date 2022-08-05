(ns hartnaeckig.bag)

(defn bag [] {:size 0 :content {}})

(defn add [bag ele]
  (-> bag
      (update :size inc)
      (update-in [:content ele] (fnil + 0) 1)))

(defn rmv [{content :content :as bag} ele]
  (let [cnt (get content ele)]
    (-> bag
        (update :size dec)
        (assoc :content (cond
                          (nil? cnt) (throw (ex-info "No such element" {:ele ele}))
                          (= cnt 1) (dissoc content ele)
                          :else (update content ele - 1))))))

(defn contains?' [bag ele]
  (some? (get-in bag [:content ele])))

(defn count' [bag ele]
  (get-in bag [:content ele]))

(defn as-set [bag]
  (-> bag :content keys set))

(defn size [bag] (:size bag))

(comment
  (def b (-> (bag) (add "foo") (add "foo") (rmv "foo") (add "bar")))
  (contains?' b "foo")
  (count' b "foo")
  (as-set b)
  (size b))

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
  (count b)
  (get b "foo")
  (contains? b "foo")
  (seq b)
  (b "foo"))
