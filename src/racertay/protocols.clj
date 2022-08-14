(ns racertay.protocols
  (:require [racertay.matrix :as matrix]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.color :as color]))

(defprotocol Shape
  (local-normal-at [shape point])
  (local-intersect [shape local-ray]))

(defn shape-data []
  {:id (java.util.UUID/randomUUID)
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix
   :material #:material{:color color/white
                        :ambient 0.1
                        :diffuse 0.9
                        :specular 0.9
                        :shininess 200.0
                        :reflective 0.0}})

(defn apply-transform
  ([obj xform]
   (let [updated (update obj :transform (partial matrix/mat-mul xform))]
     (assoc updated :inverse-transform (matrix/inverse (:transform updated)))))
  
  ([obj xform & more]
   (reduce apply-transform obj (cons xform more))))

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

(defprotocol Pattern
  (pattern-at [pattern point]))

(def pattern-data
  {:transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix})

(defn pattern-at-shape [pattern shape point]
  (let [obj-point (matrix/mat-mul-tup (:inverse-transform shape) point)
        pat-point (matrix/mat-mul-tup (:inverse-transform pattern) obj-point)]
    (pattern-at pattern pat-point)))
