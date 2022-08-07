(ns racertay.protocols
  (:require [racertay.matrix :as matrix]
            [racertay.material :as material]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]))

(defprotocol Shape
  (local-normal-at [shape point])
  (local-intersect [shape local-ray]))

(defn shape-data []
  {:id (java.util.UUID/randomUUID)
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix
   :material material/new-material})

(defn apply-transform
  ([shape xform]
   (let [updated (update shape :transform (partial matrix/mat-mul xform))]
     (assoc updated :inverse-transform (matrix/inverse (:transform updated)))))
  
  ([shape xform & more]
   (reduce apply-transform shape (cons xform more))))

(defn intersect [shape ray]
  (let [local-ray (ray/transform ray (:inverse-transform shape))]
    (local-intersect shape local-ray)))

(defn normal-at [shape point]
  (let [{:keys [inverse-transform]} shape
        local-point (matrix/mat-mul-tup inverse-transform point)
        local-normal (local-normal-at shape local-point)
        world-normal (matrix/mat-mul-tup
                      (matrix/transpose inverse-transform) local-normal)]
    (tup/normalize
       (tup/vect (tup/x world-normal) (tup/y world-normal) (tup/z world-normal)))))
