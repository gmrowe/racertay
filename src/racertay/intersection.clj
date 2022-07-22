(ns racertay.intersection)

(defn intersection [t obj]
  {:t t
   :object obj})

(defn t [inter]
  (:t inter))

(defn object [inter]
  (:object inter))

(def intersections vector)
