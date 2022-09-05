(ns racertay.shapes.sphere
  (:require
   [clojure.math :as math]
   [racertay.intersection :as inter]
   [racertay.protocols :as p]
   [racertay.ray :as ray]
   [racertay.tuple :as tup]))

(def world-origin (tup/point 0 0 0))

(defrecord ISphere
    []
  p/Shape
  (local-normal-at [sphere local-point]
    (tup/tup-sub local-point world-origin))

  (local-intersect [sphere local-ray] 
    (let [{:ray/keys [origin direction]} local-ray
          sphere-to-ray-vec (tup/tup-sub origin world-origin)
          a (tup/dot direction direction)
          b (* 2 (tup/dot direction sphere-to-ray-vec))
          c (dec (tup/dot sphere-to-ray-vec sphere-to-ray-vec))
          discriminant (- (* b b) (* 4 a c))]
      (if (neg? discriminant)
        inter/empty-intersections
        (inter/intersections
         (inter/intersection (/ (- (- b) (math/sqrt discriminant)) (* a 2)) sphere)
         (inter/intersection (/ (+ (- b) (math/sqrt discriminant)) (* a 2)) sphere))))))


