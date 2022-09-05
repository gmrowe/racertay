(ns racertay.shapes.cube
  (:require
   [racertay.fcmp :as fcmp]
   [racertay.intersection :as inter]
   [racertay.protocols :as p]
   [racertay.ray :as ray]
   [racertay.tuple :as tup]))


(defn- check-axis
  [origin direction]
  (let [tmin-num (- -1 origin)
        tmax-num (- 1 origin)]
    (sort
     (if (< fcmp/epsilon (abs direction))
       [(/ tmin-num direction) (/ tmax-num direction)]
       [(* tmin-num ##Inf) (* tmax-num ##Inf)]))))

(defrecord ICube
    []
  p/Shape
  (local-normal-at [cube local-point]
    (let [x (tup/x local-point)
          y (tup/y local-point)
          z (tup/z local-point)
          maxc (max (abs x) (abs y) (abs z))]
      (condp = maxc
        (abs x) (tup/vect x 0 0)
        (abs y) (tup/vect 0 y 0)
        (abs z) (tup/vect 0 0 z))))
  
  (local-intersect [cube local-ray]
    (let [{:ray/keys [origin direction]} local-ray
          [xtmin xtmax] (check-axis (tup/x origin) (tup/x direction))
          [ytmin ytmax] (check-axis (tup/y origin) (tup/y direction))
          [ztmin ztmax] (check-axis (tup/z origin) (tup/z direction))
          tmin (max xtmin ytmin ztmin)
          tmax (min xtmax ytmax ztmax)]
      (if (<= tmin tmax)
        (inter/intersections
         (inter/intersection tmin cube)
         (inter/intersection tmax cube))
        inter/empty-intersections))))
