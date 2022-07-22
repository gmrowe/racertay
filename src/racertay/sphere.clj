(ns racertay.sphere
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]))

(defn sphere []
  {:id (java.util.UUID/randomUUID)})

(def world-origin (tup/point 0 0 0))

(defn intersect [s r] 
  (let [sphere-to-ray-vec (tup/tup-sub (ray/origin r) world-origin)
        a (tup/dot (ray/direction r) (ray/direction r))
        b (* 2 (tup/dot (ray/direction r) sphere-to-ray-vec))
        c (dec (tup/dot sphere-to-ray-vec sphere-to-ray-vec))
        discriminant (- (* b b) (* 4 a c))]
    (if (neg? discriminant)
      []
      [(/ (- (- b) (Math/sqrt discriminant)) (* a 2))
       (/ (+ (- b) (Math/sqrt discriminant)) (* a 2))])))
