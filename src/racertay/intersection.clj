(ns racertay.intersection
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.protocols :as p]))

(defn intersection [t obj]
  {:t t
   :object obj})

(defn t [inter]
  (:t inter))

(defn object [inter]
  (:object inter))

(def intersections vector)
(def empty-intersections (intersections))

(defn hit [inters]
  (when-let [hits (seq (filter #(pos? (t %)) inters))]
    (apply min-key t hits)))

(defn prepare-computations [inters ray]
  (let [point (ray/position ray (:t inters))
        normalv (p/normal-at (:object inters) point)
        eyev (tup/tup-neg (:direction ray))
        normalv-dot-eyev (tup/dot normalv eyev)]
    (merge inters
           {:point point
            :eyev eyev
            :normalv (if (neg? normalv-dot-eyev) (tup/tup-neg normalv) normalv)
            :inside (neg? normalv-dot-eyev)})))
