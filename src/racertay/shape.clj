(ns racertay.shape
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]
            [racertay.intersection :as inter]
            [racertay.matrix :as matrix]
            [racertay.material :as material]
            [racertay.protocols :as p]))

(def world-origin (tup/point 0 0 0))

(defrecord ISphere
    [id transform inverse-transform material]
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
         (inter/intersection (/ (- (- b) (Math/sqrt discriminant)) (* a 2)) sphere)
         (inter/intersection (/ (+ (- b) (Math/sqrt discriminant)) (* a 2)) sphere))))))

(defn sphere []
  (map->ISphere (p/shape-data)))


(defrecord IPlane
    [id transform inverse-transform material]
  p/Shape
  
  (local-normal-at [plane local-point]
    (tup/vect 0 1 0))
  
  (local-intersect [plane local-ray]
    (let [{:ray/keys [origin direction]} local-ray]
      (if (< fcmp/epsilon (Math/abs (tup/y direction)))
        (let [t (/ (- (tup/y origin)) (tup/y direction))]
          (inter/intersections (inter/intersection t plane)))
        inter/empty-intersections))))

(defn plane []
  (map->IPlane (p/shape-data)))
