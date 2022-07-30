(ns racertay.sphere
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.intersection :as inter]
            [racertay.matrix :as matrix]
            [racertay.material :as material]
            [racertay.protocols :as p]))

(defn apply-transform
  ([sphere xform]
   (let [updated (update sphere :transform (partial matrix/mat-mul xform))]
     (assoc updated :inverse-transform (matrix/inverse (:transform updated)))))
  
  ([sphere xform & more]
   (reduce apply-transform sphere (cons xform more))))

(def world-origin (tup/point 0 0 0))

(defrecord ISphere
  [id transform inverse-transform material]
  p/Shape
  (intersect [sphere ray] 
    (let [r (ray/transform ray (:inverse-transform sphere))
          sphere-to-ray-vec (tup/tup-sub (ray/origin r) world-origin)
          a (tup/dot (ray/direction r) (ray/direction r))
          b (* 2 (tup/dot (ray/direction r) sphere-to-ray-vec))
          c (dec (tup/dot sphere-to-ray-vec sphere-to-ray-vec))
          discriminant (- (* b b) (* 4 a c))]
      (if (neg? discriminant)
        inter/empty-intersections
        (inter/intersections
         (inter/intersection (/ (- (- b) (Math/sqrt discriminant)) (* a 2)) sphere)
         (inter/intersection (/ (+ (- b) (Math/sqrt discriminant)) (* a 2)) sphere)))))

  (normal-at [sphere point]
    (let [object-point (matrix/mat-mul-tup (:inverse-transform sphere) point)
          object-normal (tup/tup-sub object-point world-origin)
          world-normal (matrix/mat-mul-tup
                        (matrix/transpose (:inverse-transform sphere))
                        object-normal)]
      (tup/normalize
       (tup/vect (tup/x world-normal) (tup/y world-normal) (tup/z world-normal))))))

(defn sphere []
  (->ISphere
   (java.util.UUID/randomUUID)
   matrix/identity-matrix
   matrix/identity-matrix
   material/new-material))
