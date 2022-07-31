(ns racertay.ray
  (:require [racertay.tuple :as tup]
            [racertay.matrix :as matrix]))

(defn ray [origin-pt direction-vec]
  #:ray{:origin origin-pt
        :direction direction-vec})

(defn distance [r t]
  (tup/tup-mul-scalar (:ray/direction r) t))

(defn position [r t]
  (tup/tup-add (:ray/origin r) (distance r t)))

(defn transform [r m]
  (ray (matrix/mat-mul-tup m (:ray/origin r))
       (matrix/mat-mul-tup m (:ray/direction r))))
