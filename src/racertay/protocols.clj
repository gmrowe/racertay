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
