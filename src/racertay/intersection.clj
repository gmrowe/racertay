(ns racertay.intersection
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]
            [racertay.protocols :as p]))

(defn intersection [t obj]
  #:intersection{:t t
                 :object obj})

(def intersections vector)
(def empty-intersections (intersections))

(defn hit [inters]
  (when-let [hits (seq (filter #(pos? (:intersection/t %)) inters))]
    (apply min-key :intersection/t hits)))

(defn prepare-computations [inters ray]
  (let [point (ray/position ray (:intersection/t inters))
        normalv (p/normal-at (:intersection/object inters) point)
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
