(ns racertay.sphere
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.intersection :as inter]
            [racertay.matrix :as matrix]))

(defn sphere []
  {:id (java.util.UUID/randomUUID)
   :transform matrix/identity-matrix})

(defn transform [sphere]
  (:transform sphere))

(defn apply-transform [sphere xform]
  (update-in sphere [:transform] (partial matrix/mat-mul xform)))

(def world-origin (tup/point 0 0 0))

(defn intersect [sphere ray] 
  (let [r (ray/transform ray (matrix/inverse (transform sphere)))
        sphere-to-ray-vec (tup/tup-sub (ray/origin r) world-origin)
        a (tup/dot (ray/direction r) (ray/direction r))
        b (* 2 (tup/dot (ray/direction r) sphere-to-ray-vec))
        c (dec (tup/dot sphere-to-ray-vec sphere-to-ray-vec))
        discriminant (- (* b b) (* 4 a c))]
    (if (neg? discriminant)
      (inter/intersections)
      (inter/intersections
       (inter/intersection (/ (- (- b) (Math/sqrt discriminant)) (* a 2)) sphere)
       (inter/intersection (/ (+ (- b) (Math/sqrt discriminant)) (* a 2)) sphere)))))
