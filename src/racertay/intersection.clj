(ns racertay.intersection)

(defn intersection [t obj]
  {:t t
   :object obj})

(defn t [inter]
  (:t inter))

(defn object [inter]
  (:object inter))

(def intersections vector)

(defn hit [inters]
  (when-let [hits (seq (filter #(pos? (t %)) inters))]
    (apply min-key t hits)))
