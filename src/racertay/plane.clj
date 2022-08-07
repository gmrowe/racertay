(ns racertay.plane
  (:require [racertay.protocols :as p]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]
            [racertay.intersection :as inter]))

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
