(ns racertay.protocols
  (:require [racertay.matrix :as matrix]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.color :as color]))

(defprotocol Shape
  (local-normal-at [shape point])
  (local-intersect [shape local-ray]))

(defprotocol Pattern
  (pattern-at [pattern point]))

(def pattern-data
  {:transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix})

(defn pattern-at-shape [pattern shape point]
  (let [obj-point (matrix/mat-mul-tup (:inverse-transform shape) point)
        pat-point (matrix/mat-mul-tup (:inverse-transform pattern) obj-point)]
    (pattern-at pattern pat-point)))
