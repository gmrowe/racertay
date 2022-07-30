(ns racertay.intersection
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.protocols :as p]))

(defn intersection [t obj]
  #:intersection{:t t
                 :object obj})

(defn t [inter]
  (:intersection/t inter))

(defn object [inter]
  (:intersection/object inter))

(def intersections vector)
(def empty-intersections (intersections))

(defn hit [inters]
  (when-let [hits (seq (filter #(pos? (t %)) inters))]
    (apply min-key t hits)))

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
