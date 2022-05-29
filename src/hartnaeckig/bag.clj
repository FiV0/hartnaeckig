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
