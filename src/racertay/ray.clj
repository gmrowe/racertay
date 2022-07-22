(ns racertay.ray
  (:require [racertay.tuple :as tup]
            [racertay.matrix :as matrix]))

(defn ray [origin-pt direction-vec]
  {:origin origin-pt
   :direction direction-vec})

(defn origin [r]
  (:origin r))

(defn direction [r]
  (:direction r))

(defn distance [r t]
  (tup/tup-mul-scalar (direction r) t))

(defn position [r t]
  (tup/tup-add (origin r) (distance r t)))

(defn transform [r m]
  (ray (matrix/mat-mul-tup m (origin r))
       (matrix/mat-mul-tup m (direction r))))
