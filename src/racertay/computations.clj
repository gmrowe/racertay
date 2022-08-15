(ns racertay.computations
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.shape :as shape]
            [racertay.fcmp :as fcmp]))

(defn prepare-computations [inters ray]
  (let [point (ray/position ray (:intersection/t inters))
        normalv (shape/normal-at (:intersection/object inters) point)
        eyev (tup/tup-neg (:ray/direction ray))
        normalv-dot-eyev (tup/dot normalv eyev)
        inside (neg? normalv-dot-eyev)
        true-normalv (if inside (tup/tup-neg normalv) normalv)
        over-point (tup/tup-add
                    point (tup/tup-mul-scalar true-normalv (/ fcmp/epsilon 2)))
        reflectv (tup/reflect (:ray/direction ray) true-normalv)]
    (merge inters
           #:intersection{:point point
                          :eyev eyev
                          :normalv true-normalv
                          :inside inside
                          :over-point over-point
                          :reflectv reflectv})))

