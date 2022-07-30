(ns racertay.intersection
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
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
        eyev (tup/tup-neg (:direction ray))
        normalv-dot-eyev (tup/dot normalv eyev)]
    (merge inters
           #:intersection{:point point
                          :eyev eyev
                          :normalv (if (neg? normalv-dot-eyev)
                                    (tup/tup-neg normalv)
                                    normalv)
                          :inside (neg? normalv-dot-eyev)})))
