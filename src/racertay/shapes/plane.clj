(ns racertay.shapes.plane
  (:require
   [racertay.fcmp :as fcmp]
   [racertay.intersection :as inter]
   [racertay.protocols :as p]
   [racertay.ray :as ray]
   [racertay.tuple :as tup]))

(defrecord IPlane
     []
  p/Shape
  
  (local-normal-at [plane local-point]
    (tup/vect 0 1 0))
  
  (local-intersect [plane local-ray]
    (let [{:ray/keys [origin direction]} local-ray]
      (if (< fcmp/epsilon (abs (tup/y direction)))
        (let [t (/ (- (tup/y origin)) (tup/y direction))]
          (inter/intersections (inter/intersection t plane)))
        inter/empty-intersections))))
