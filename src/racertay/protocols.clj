(ns racertay.protocols)

(defprotocol Shape
  (normal-at [s point])
  (intersect [s ray]))
